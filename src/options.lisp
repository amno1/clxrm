;;;; options.lisp — Xt conventions: named options backed by X resources
;;;; Copyright (C) 2026 Arthur Miller
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; An X Toolkit program takes each setting from, in order of priority,
;;;; what the caller passed, the user's resources (e.g. an
;;;; "XClock.background: navy" line loaded by xrdb), and a built-in
;;;; default.  This layer implements that for Lisp keyword options.  It
;;;; needs a resource database but no server, so it works equally with
;;;; either backend, or with a database read from a file.
;;;;
;;;; Naming follows Xt: option :MAX-WIDTH is resource maxWidth, class
;;;; MaxWidth; application xclock may have class XClock.

(in-package #:xrm)

;;; Names

(defun resource-name-for (key)
  "Return the Xt resource name for option KEY, a symbol or string:
   hyphenated words become camelCase, so :MAX-WIDTH gives \"maxWidth\"
   and :BACKGROUND gives \"background\"."
  (let ((words (uiop:split-string (string-downcase (string key)) :separator "-")))
    (format nil "~A~{~@(~A~)~}" (first words) (rest words))))

(defun resource-class-for (name)
  "Return the Xt resource class for resource NAME: NAME with its first
   letter upper-cased, so \"maxWidth\" gives \"MaxWidth\"."
  (if (zerop (length name))
      name
      (concatenate 'string (string (char-upcase (char name 0))) (subseq name 1))))

;;; Converting resource strings

(defun %boolean-type-p (type)
  (and type (subtypep type 'boolean) (subtypep 'boolean type)))

(defun %number-shaped-p (string)
  (and (plusp (length string))
       (every (lambda (c) (find c "0123456789+-./eEdD")) string)))

(defun %keyword-candidate (string type)
  "The keyword candidate for STRING in CONVERT-RESOURCE-STRING: an
   already-interned keyword with STRING's upcased name if there is one,
   and otherwise a freshly interned keyword only when a keyword could
   actually be a value of TYPE.  This keeps a resource like
   \"xclock.geometry: 80x24\" on an INTEGER option from permanently
   adding :80X24 to the :KEYWORD package."
  (let ((name (string-upcase string)))
    (multiple-value-bind (symbol status) (find-symbol name :keyword)
      (cond ((and symbol status) symbol)
            ((handler-case (subtypep 'keyword type) (error () nil))
             (intern name :keyword))
            (t nil)))))

(defun convert-resource-string (string type)
  "Convert STRING, a resource value or a command-line argument, to a
   value of TYPE.  Returns (VALUES VALUE OK-P); OK-P is NIL when no
   conversion gives a value of TYPE.

   For a BOOLEAN type the Xt words are accepted, case-insensitively:
   true yes on 1 t for T, false no off 0 nil for NIL.  For any other
   type the candidates NIL (for \"nil\"), an integer, a real, a keyword
   and STRING itself are tried in that order, and the first of TYPE
   wins; so an (OR NULL STRING) option gets NIL from \"nil\" and the
   string otherwise.  With TYPE T or NIL the string is returned as it
   is.  Only number-shaped strings are handed to the Lisp reader, with
   *READ-EVAL* off, so nothing else is interned anywhere."
  (let ((s (string-trim '(#\Space #\Tab) string)))
    (cond
      ((or (null type) (eq type t))
       (values string t))
      ((%boolean-type-p type)
       (cond ((member s '("true" "yes" "on" "1" "t") :test #'string-equal)
              (values t t))
             ((member s '("false" "no" "off" "0" "nil") :test #'string-equal)
              (values nil t))
             (t (values nil nil))))
      (t
       (let ((candidates
               (remove :none
                       (list (if (string-equal s "nil")
                                 nil
                                 :none)
                             (handler-case (parse-integer s)
                               (error () :none))
                             (if (%number-shaped-p s)
                                 (let ((*read-default-float-format* 'double-float)
                                       (*read-eval* nil))
                                   (handler-case
                                       (let ((v (read-from-string s)))
                                         (if (realp v)
                                             v
                                             :none))
                                     (error () :none)))
                                 :none)
                             ;; %KEYWORD-CANDIDATE returns NIL for "no keyword
                             ;; candidate"; map it to :NONE so REMOVE drops it.
                             (or (%keyword-candidate s type) :none)
                             string))))
         (dolist (c candidates (values nil nil))
           (when (typep c type)
             (return (values c t)))))))))

;;; Options

(defstruct (option (:constructor %make-option))
  (key nil :type symbol)
  (resource-name "" :type string)
  (resource-class "" :type string)
  (default nil)
  (type t))

(defun make-option (key &key resource-name resource-class default (type t))
  "Return an option spec for KEY.  RESOURCE-NAME defaults to
   (RESOURCE-NAME-FOR KEY), RESOURCE-CLASS to the class of that name.
   DEFAULT is the value when neither the caller nor the resources give
   one; TYPE is what a resource string is converted to."
  (let ((rname (or resource-name (resource-name-for key))))
    (%make-option :key key
                  :resource-name rname
                  :resource-class (or resource-class (resource-class-for rname))
                  :default default
                  :type type)))

(defun lookup-resource-value (database names classes resource-name resource-class type)
  "Look RESOURCE-NAME / RESOURCE-CLASS up in DATABASE under the
   application path NAMES / CLASSES (lists, e.g. (\"xclock\") and
   (\"XClock\")), and convert the value to TYPE.  Returns
   (VALUES VALUE FOUNDP).  A value that does not convert is reported
   with a warning and treated as not found, so a typo in a resource
   file cannot break the program.  DATABASE may be NIL."
  (if (null database)
      (values nil nil)
      (multiple-value-bind (string foundp)
          (xrm:get-resource database resource-name resource-class names classes)
        (cond
          ((not foundp) (values nil nil))
          ((not (stringp string)) (values string t)) ; already converted by a KEY
          (t
           (multiple-value-bind (value okp) (convert-resource-string string type)
             (if okp
                 (values value t)
                 (progn
                   (warn "Ignoring X resource ~{~A~^.~}.~A: ~S is not a valid ~S."
                         names resource-name string type)
                   (values nil nil)))))))))

(defun lookup-option (database names classes options key &optional plist)
  "Return the value of option KEY, one of OPTIONS (a list of OPTION
   specs): from PLIST, the caller's keyword arguments, if KEY is present
   there (even with value NIL); else from DATABASE under the application
   path NAMES / CLASSES; else the option's default.  The second value
   says where it came from: :ARGUMENT, :RESOURCE or :DEFAULT."
  (let ((spec (find key options :key #'option-key)))
    (unless spec
      (error "No option ~S declared." key))
    (multiple-value-bind (indicator value) (get-properties plist (list key))
      (if indicator
          (values value :argument)
          (multiple-value-bind (from-resource foundp)
              (lookup-resource-value database names classes
                                     (option-resource-name spec)
                                     (option-resource-class spec)
                                     (option-type spec))
            (if foundp
                (values from-resource :resource)
                (values (option-default spec) :default)))))))

(defmacro define-option-set (name &body specs)
  "Define NAME as a list of Xt-style option specs.  Each SPEC is
   (KEY &key default type documentation resource-name resource-class),
   as for MAKE-OPTION, evaluated at load time."
  `(defparameter ,name
     (list
      ,@(loop for (key . options) in specs
              collect `(make-option ,key ,@options)))))
