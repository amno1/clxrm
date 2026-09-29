;;;; resources.lisp — reading and writing resource files
;;;;
;;;; A port of xcb-util-xrm's database reader and writer (src/database.c,
;;;; src/entry.c, src/util.c).  The notice of xcb-util-xrm follows and
;;;; must be kept:
;;;
;;; Copyright © 2016 Ingo Bürk
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining a
;;; copy of this software and associated documentation files (the "Software"),
;;; to deal in the Software without restriction, including without limitation
;;; the rights to use, copy, modify, merge, publish, distribute, sublicense,
;;; and/or sell copies of the Software, and to permit persons to whom the
;;; Software is furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be included in
;;; all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;;; AUTHORS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
;;;
;;; Except as contained in this notice, the names of the authors or their
;;; institutions shall not be used in advertising or otherwise to promote the
;;; sale, use or other dealings in this Software without prior written
;;; authorization from the authors.
;;;
;;;; Port: Copyright (C) 2026 Arthur Miller
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The lambda lists are CLX's, and so is the meaning of a string
;;;; argument: a pathname, not resource text (use
;;;; READ-RESOURCES-FROM-STRING for that).  The syntax is xcb-util-xrm's:
;;;;
;;;;   * Every backslash followed by a newline is removed first; the text
;;;;     is then split into lines, and empty lines are dropped.
;;;;
;;;;   * A line starting with # followed, after optional whitespace, by
;;;;     "include" includes a file, resolved against the directory of the
;;;;     including file, or of *DEFAULT-PATHNAME-DEFAULTS* when there is
;;;;     none.  A missing file is ignored, and includes nest at most 100
;;;;     deep; deeper ones are ignored.
;;;;
;;;;   * Any other line starting with ! or # is a comment, and so is, in
;;;;     effect, every line that does not parse as an entry: invalid lines
;;;;     are ignored.
;;;;
;;;;   * In a value, \<space>, \<tab>, \\, \n and \ooo (exactly three
;;;;     octal digits) are escapes; any other backslash is kept.
;;;;     Whitespace after the colon is dropped; trailing whitespace is
;;;;     kept.

(in-package #:xrm)

(defconstant +max-include-depth+ 100)

(defmacro with-resource-stream ((stream source &rest open-options) &body body)
  "Bind STREAM to SOURCE if it is a stream, or else to SOURCE opened as a
   file with OPEN-OPTIONS, closing it afterwards (aborting on a non-local
   exit)."
  (let ((abortp (gensym "ABORTP"))
        (streamp (gensym "STREAMP"))
        (src (gensym "SOURCE")))
    `(let* ((,src ,source)
            (,abortp t)
            (,streamp (streamp ,src))
            (,stream (if ,streamp ,src (open ,src ,@open-options))))
       (unwind-protect
            (multiple-value-prog1
                (progn ,@body)
              (setq ,abortp nil))
         (unless ,streamp
           (close ,stream :abort ,abortp))))))

;; __xcb_xrm_entry_escape_value: escape a leading space or tab,
;; newlines and backslashes.
(defun %escape-value (string)
  (with-output-to-string (out)
    (when (and (plusp (length string))
               (member (char string 0) '(#\Space #\Tab)))
      (write-char #\\ out))
    (loop for c across string
          do (case c
               (#\Newline (write-string "\\n" out))
               (#\\ (write-string "\\\\" out))
               (t (write-char c out))))))

;; Remove every backslash followed by a newline, left to right.
(defun %remove-continuations (string)
  (let ((marker (load-time-value (coerce '(#\\ #\Newline) 'string))))
    (if (not (search marker string))
        string
        (with-output-to-string (out)
          (let ((n (length string)) (i 0))
            (loop while (< i n)
                  do (if (and (char= (char string i) #\\)
                              (< (1+ i) n)
                              (char= (char string (1+ i)) #\Newline))
                         (incf i 2)
                         (progn
                           (write-char (char string i) out)
                           (incf i)))))))))

;; Split STRING on newlines, dropping empty lines, as strtok does.
(defun %split-lines (string)
  (loop with n = (length string)
        for start = 0 then (1+ end)
        for end = (or (position #\Newline string :start start) n)
        when (> end start) collect (subseq string start end)
        while (< end n)))

;; If LINE is an include directive to be followed at DEPTH, return
;; (VALUES T FILE-NAME), FILE-NAME being NIL when there is none;
;; otherwise NIL.
(defun %include-file-name (line depth)
  (when (and (plusp (length line)) (char= (char line 0) #\#)
             (< depth +max-include-depth+))
    (let* ((i (or (position-if-not (lambda (c) (member c '(#\Space #\Tab))) line :start 1)
                  (length line))))
      (when (and (<= (+ i 7) (length line))
                 (string= "include" line :start2 i :end2 (+ i 7)))
        (let* ((trim '(#\Space #\Tab #\"))
               (start (or (position-if-not (lambda (c) (member c trim)) line :start (+ i 7))
                          (length line)))
               (end (1+ (or (position-if-not (lambda (c) (member c trim)) line :from-end t)
                            -1))))
          (values t (and (< start end) (subseq line start end))))))))

;; resolve_path: NAME if absolute, else NAME in directory
(defun %resolve-path (name base)
  (if (and (plusp (length name)) (char= (char name 0) #\/))
      (pathname name)
      (merge-pathnames name base)))

(defun %stream-base (stream)
  (let ((file (and (typep stream 'file-stream) (ignore-errors (truename stream)))))
    (if file
        (make-pathname :name nil :type nil :version nil :defaults file)
        (make-pathname :name nil :type nil :version nil
                       :defaults *default-pathname-defaults*))))
(defun %read-all-octets (stream)
  "Read STREAM to its end and return a SIMPLE-ARRAY (UNSIGNED-BYTE 8) (*).
   Read in 4 KiB chunks straight into a growing destination with
   READ-SEQUENCE -- no per-byte fill-pointer bookkeeping -- doubling
   when the buffer fills; if the stream is a file whose length is
   known, size the buffer once."
  (let* ((known (and (typep stream 'file-stream)
                     (ignore-errors (file-length stream))))
         (buffer (make-array (or known 4096)
                             :element-type '(unsigned-byte 8)
                             :adjustable t))
         (size (array-dimension buffer 0))
         (end 0))
    (loop
      (when (= end size)
        (setf size (* 2 size))
        (adjust-array buffer size))
      (let ((new-end (read-sequence buffer stream :start end :end size)))
        (cond ((= new-end end) (return))          ; EOF
              ((< new-end size)                   ; short read: EOF
               (setf end new-end)
               (return))
              (t (setf end new-end)))))
    (subseq buffer 0 end)))

(defun %read-stream-text (stream)
  "The whole contents of STREAM as a string.  A binary stream is read
   as bytes and decoded as UTF-8, or as Latin-1 if the bytes are not
   valid UTF-8 (OCTETS-TO-STRING); a character stream is read as it
   is."
  (if (subtypep (stream-element-type stream) 'character)
      (with-output-to-string (out)
        (let ((buffer (make-string 4096)))
          (loop for n = (read-sequence buffer stream)
                while (plusp n)
                do (write-string buffer out :end n))))
      (octets-to-string (%read-all-octets stream))))

(defun %read-file-text (file)
  "The contents of FILE decoded as by %READ-STREAM-TEXT, or NIL if it
   cannot be read."
  (ignore-errors
   (with-open-file (in file :element-type '(unsigned-byte 8))
     (%read-stream-text in))))

(defun %read-resource-text (database text base depth key test test-not)
  (dolist (line (%split-lines (%remove-continuations text)))
    (multiple-value-bind (include-p name) (%include-file-name line depth)
      (cond
        (include-p
         (when name
           (let ((file (%resolve-path name base)))
             (when (ignore-errors (probe-file file))
               (let ((contents (%read-file-text file)))
                 (when contents
                   (%read-resource-text database contents
                                        (make-pathname :name nil :type nil :version nil
                                                       :defaults file)
                                        (1+ depth) key test test-not)))))))
        ((member (char line 0) '(#\! #\#)))
        (t
         (multiple-value-bind (components value) (%parse-entry line)
           (when components
             (when key (setq value (funcall key value)))
             (when (cond (test (funcall test (%components-name-list components) value))
                         (test-not (not (funcall test-not (%components-name-list components)
                                                 value)))
                         (t t))
               (%put database components value)))))))))

(defun read-resources (database pathname &key key test test-not)
  "Merge the resources in PATHNAME, in standard X11 resource file
   format, into DATABASE, overriding entries of the same name, as
   xcb-util-xrm's xcb_xrm_database_from_file and
   xcb_xrm_database_combine do.  Returns DATABASE.  PATHNAME may be a
   pathname, a string naming a file, or an input stream.  A file, or a
   binary input stream, is read as bytes and decoded as UTF-8, or as
   Latin-1 if it is not valid UTF-8, as the RESOURCE_MANAGER property
   is; a character stream is read with its own external format.

   KEY, if given, converts each value string.  TEST and TEST-NOT are
   predicates for filtering which resources are added; they are called
   with the name list and the (converted) value."
  (declare (type resource-database database)
           (type (or pathname string stream) pathname)
           (type (or null function) key test test-not))
  (with-resource-stream (stream pathname :element-type '(unsigned-byte 8))
    (%read-resource-text database (%read-stream-text stream) (%stream-base stream)
                         0 key test test-not))
  database)

(defun read-resources-from-string (database string &key key test test-not)
  "Merge the resource text STRING (the contents of a resource file, or
   of the RESOURCE_MANAGER property) into DATABASE, as
   xcb_xrm_database_from_string and xcb_xrm_database_combine do, and
   return DATABASE.  KEY, TEST and TEST-NOT are as for READ-RESOURCES."
  (declare (type string string))
  (%read-resource-text database string
                       (make-pathname :name nil :type nil :version nil
                                      :defaults *default-pathname-defaults*)
                       0 key test test-not)
  database)

(defun parse-resource (string &optional (start 0) end)
  "Parse a resource line, as xcb_xrm_entry_parse does, into
   (VALUES NAME-LIST VALUE).  Names are strings; a loose binding is the
   string \"*\" and a wildcard the string \"?\".  Returns NIL for an
   invalid line."
  (declare (type string string)
           (type fixnum start)
           (type (or null fixnum) end))
  (multiple-value-bind (components value)
      (%parse-entry (subseq string start end))
    (if components
        (values (%components-name-list components) value)
        (values nil nil))))

(defun put-resource-line (database line)
  "Add the resource on LINE, one line of a resource file such as
   \"XClock.background: navy\", to DATABASE, as xcb-util-xrm's
   xcb_xrm_database_put_resource_line does: a line starting with ! or #
   and an invalid line are ignored.  Returns the value added, or NIL."
  (declare (type resource-database database)
           (type string line))
  (unless (and (plusp (length line)) (member (char line 0) '(#\! #\#)))
    (multiple-value-bind (components value) (%parse-entry line)
      (when components
        (%put database components value)
        value))))

(defun put-resource (database specifier value)
  "Add VALUE, a string, to DATABASE under SPECIFIER, a resource name such
   as \"XClock*hands\", as xcb-util-xrm's xcb_xrm_database_put_resource
   does: VALUE is escaped, joined to SPECIFIER with \": \" and added with
   PUT-RESOURCE-LINE, so an invalid SPECIFIER adds nothing.  Returns the
   value added, or NIL."
  (declare (type resource-database database)
           (type string specifier value))
  (put-resource-line database (concatenate 'string specifier ": " (%escape-value value))))

(defun %write-value (value stream)
  (write-string (%escape-value (princ-to-string value)) stream))

(defun write-resources (database pathname &key write test test-not)
  "Write DATABASE to PATHNAME, a pathname, a string naming a file, or an
   output stream, as xcb-util-xrm's xcb_xrm_database_to_string does.  An
   existing file is superseded.  WRITE is a function of (VALUE STREAM)
   that writes a value; the default writes it as PRINC would, escaped
   for the file format.  TEST and TEST-NOT filter which resources are
   written; they are called with the name list and the value."
  (declare (type resource-database database)
           (type (or pathname string stream) pathname)
           (type (or null function) write test test-not))
  (let ((write (or write #'%write-value)))
    (with-resource-stream (stream pathname :direction :output
                                           :if-exists :supersede
                                           :if-does-not-exist :create)
      (do-entries (entry database)
        (let ((name-list (%components-name-list (entry-components entry)))
              (value (entry-value entry)))
          (when (cond (test (funcall test name-list value))
                      (test-not (not (funcall test-not name-list value)))
                      (t t))
            (let ((line (%entry-string (entry-components entry) "")))
              (write-string line stream))
            (funcall write value stream)
            (terpri stream))))))
  database)

(defun write-resources-to-string (database &key write test test-not)
  "Return DATABASE in standard X11 resource file format, as a string.
   WRITE, TEST and TEST-NOT are as for WRITE-RESOURCES."
  (with-output-to-string (stream)
    (write-resources database stream :write write :test test :test-not test-not)))
