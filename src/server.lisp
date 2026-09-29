;;;; server.lisp — resources stored on the X server
;;;; Copyright (C) 2026 Arthur Miller
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The RESOURCE_MANAGER property of a screen's root window holds the
;;;; user's resources, as loaded there by xrdb.  These functions read and
;;;; write it through the backend protocol, so they work with any X
;;;; library that has a backend.  They take the connection (the
;;;; "context") explicitly, because an XCB window is a bare integer that
;;;; does not know its connection.  CLX-shaped versions, taking a window
;;;; or screen alone, are in CLXRM/CLX.

(in-package #:xrm)

;;; RESOURCE_MANAGER

(declaim (inline %screen-root))
(defun %screen-root (context screen)
  (backend-screen-root (context-backend context) context
                       (or screen (context-default-screen context))))

(defun wm-resources (context window &key database key test test-not)
  "Merge the RESOURCE_MANAGER property of WINDOW on CONTEXT into
   DATABASE (a fresh database if NIL) and return the database.  WINDOW
   is in the backend's representation.  KEY, TEST and TEST-NOT are as
   for READ-RESOURCES.  A window without the property leaves DATABASE
   unchanged."
  (let ((database (or database (make-resource-database))))
    (let ((data (backend-get-property (context-backend context) context
                                      window "RESOURCE_MANAGER")))
      (when data
        (read-resources-from-string database (octets-to-string data)
                                    :key key :test test :test-not test-not)))
    database))

(defun set-wm-resources (context window database &key write test test-not)
  "Store DATABASE, in resource file format, as the RESOURCE_MANAGER
   property of WINDOW on CONTEXT, replacing its contents.  WRITE, TEST
   and TEST-NOT are as for WRITE-RESOURCES.  Returns DATABASE."
  (backend-set-property (context-backend context) context window
                        "RESOURCE_MANAGER" "STRING" 8
                        (string-to-octets
                         (write-resources-to-string database :write write
                                                             :test test
                                                             :test-not test-not)))
  database)

(defun root-resources (context &key screen database key test test-not)
  "Return a resource database with the RESOURCE_MANAGER property of the
   root window of SCREEN (by default CONTEXT's default screen), which is
   where xrdb loads the user's resources.  If DATABASE is given the
   resources are merged into it and it is returned.  KEY, TEST and
   TEST-NOT are as for READ-RESOURCES."
  (wm-resources context (%screen-root context screen)
                :database database :key key :test test :test-not test-not))

(defun set-root-resources (context database &key screen write test test-not)
  "Store DATABASE as the RESOURCE_MANAGER property of SCREEN's root
   window (by default CONTEXT's default screen), as xrdb -load does.
   Returns DATABASE."
  (set-wm-resources context (%screen-root context screen) database
                    :write write :test test :test-not test-not))

;;; The Xlib default database

(defun xresources-pathname ()
  "The user's resource file on current systems, read when the server has
   no RESOURCE_MANAGER property: ~/.Xresources."
  (merge-pathnames ".Xresources" (user-homedir-pathname)))

(defun default-resources-pathname ()
  "The traditional user resource file, read when the server has no
   RESOURCE_MANAGER property and there is no ~/.Xresources:
   ~/.Xdefaults."
  (merge-pathnames ".Xdefaults" (user-homedir-pathname)))

(defun resources-pathname ()
  "The per-host resource file read after the defaults: $XENVIRONMENT
   if set, otherwise ~/.Xdefaults-<hostname>."
  (let ((env (uiop:getenv "XENVIRONMENT")))
    (if (and env (plusp (length env)))
        (pathname env)
        (merge-pathnames (concatenate 'string ".Xdefaults-" (machine-instance))
                         (user-homedir-pathname)))))

(defun default-resources (context &key screen database)
  "Return the resource database a client starts from, as xcb-util-xrm's
   xcb_xrm_database_from_default builds it: the RESOURCE_MANAGER
   property of SCREEN's root window (what xrdb loaded) if it exists,
   otherwise ~/.Xresources, otherwise ~/.Xdefaults; and then
   $XENVIRONMENT, or ~/.Xdefaults-<hostname>, merged on top.  Missing
   files are skipped.  If DATABASE is given the resources are merged
   into it.

   Xlib's XrmGetDatabase and CLX's INITIALIZE-RESOURCE-DATABASE do not
   try ~/.Xresources, but that is where current desktops keep the
   user's resources, for sessions that have not run xrdb."
  (let* ((database (or database (make-resource-database)))
         (data (backend-get-property (context-backend context) context
                                     (%screen-root context screen)
                                     "RESOURCE_MANAGER")))
    (if data
        (read-resources-from-string database (octets-to-string data))
        (let ((path (find-if #'probe-file (list (xresources-pathname)
                                                (default-resources-pathname)))))
          (when path
            (read-resources database path))))
    (let ((path (resources-pathname)))
      (when (probe-file path)
        (read-resources database path)))
    database))
