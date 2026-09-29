;;;; clx.lisp — clXrm backend for CLX
;;;; Copyright (C) 2026 Arthur Miller
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The context is an XLIB:DISPLAY, a screen an XLIB:SCREEN, a window an
;;;; XLIB:WINDOW.

(defpackage #:clxrm/clx
  (:use #:cl)
  (:export #:clx-backend
           #:set-root-resources
           #:root-resources
           #:set-wm-resources
           #:wm-resources)
  (:documentation "clXrm backend for CLX: lets the clXrm server
   functions take an XLIB:DISPLAY as their context."))

(in-package #:clxrm/clx)

(defclass clx-backend (xrm:backend) ())

(defparameter *backend* (make-instance 'clx-backend))

(defmethod xrm:context-backend ((context xlib:display))
  *backend*)

(defmethod xrm:context-default-screen ((context xlib:display))
  (xlib:display-default-screen context))

(defmethod xrm:backend-screen-root ((backend clx-backend) context (screen xlib:screen))
  (declare (ignore context))
  (xlib:screen-root screen))

(defmethod xrm:backend-get-property ((backend clx-backend) context window name)
  (declare (ignore context))
  (multiple-value-bind (data type format)
      (xlib:get-property window
                         (xlib:intern-atom (xlib:window-display window) name)
                         :result-type 'vector)
    ;; CLX decodes format 16 and 32 items into integers; only format 8
    ;; is raw octets, and that is all the core reads.
    (if (and data (plusp (length data)) (eql format 8))
        (values (coerce data '(simple-array (unsigned-byte 8) (*))) format type)
        (values nil nil nil))))

(defmethod xrm:backend-set-property ((backend clx-backend) context window
                                        name type format data)
  (declare (ignore context))
  ;; CLX takes FORMAT 16 and 32 data as items, not octets.
  (check-type format (eql 8))
  (xlib:change-property window
                        (xlib:intern-atom (xlib:window-display window) name)
                        data
                        (if (stringp type)
                            (xlib:intern-atom (xlib:window-display window) type)
                            type)
                        format)
  ;; FINISH-OUTPUT, not FORCE-OUTPUT: wait until the server has done it.
  (xlib:display-finish-output (xlib:window-display window)))

(defmethod xrm:backend-intern-atom ((backend clx-backend) (display xlib:display) name)
  (xlib:intern-atom display name))

;;; CLX-shaped conveniences over XRM:ROOT-RESOURCES etc.

(defun %screen (screen)
  (if (typep screen 'xlib:display)
      (xlib:display-default-screen screen)
      screen))

(defun wm-resources (database window &key key test test-not)
  "Merge the RESOURCE_MANAGER property of WINDOW, an XLIB:WINDOW, into
   DATABASE and return DATABASE.  As in CLX."
  (xrm:wm-resources (xlib:window-display window) window
                       :database database :key key :test test :test-not test-not))

(defun set-wm-resources (database window &key write test test-not)
  "Store DATABASE as the RESOURCE_MANAGER property of WINDOW, an
   XLIB:WINDOW.  As in CLX."
  (xrm:set-wm-resources (xlib:window-display window) window database
                           :write write :test test :test-not test-not))

(defun root-resources (screen-or-display &key database key test test-not)
  "Return a resource database with the RESOURCE_MANAGER property of
   SCREEN-OR-DISPLAY's root window; SCREEN may be an XLIB:SCREEN or an
   XLIB:DISPLAY, meaning its default screen.  If DATABASE is given the
   resources are merged into it.  As in CLX."
  (let ((screen (%screen screen-or-display)))
    (wm-resources (or database (xrm:make-resource-database))
                  (xlib:screen-root screen)
                  :key key :test test :test-not test-not)))

(defun set-root-resources (screen &key test test-not write database)
  "Store DATABASE as the RESOURCE_MANAGER property of SCREEN's root
   window; SCREEN may be an XLIB:SCREEN or an XLIB:DISPLAY.  Returns
   DATABASE.  As in CLX, except that WRITE defaults to clXrm's escaping
   writer rather than PRINC, so values survive a round trip."
  (set-wm-resources database (xlib:screen-root (%screen screen))
                    :write write :test test :test-not test-not)
  database)

(defsetf root-resources (screen &key test test-not write) (database)
  `(set-root-resources ,screen :test ,test :test-not ,test-not
                               :write ,write :database ,database))
