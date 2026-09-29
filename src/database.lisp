;;;; database.lisp — resource databases and the Xrm matching algorithm
;;;; Copyright (C) 2026 Arthur Miller
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A port of the entry parser, database and matching algorithm of
;;;; xcb-util-xrm (src/entry.c, src/database.c, src/match.c), so that a
;;;; database gives exactly the results xcb-util-xrm gives (minor the
;;;; '?' bug).  xcb-util-xrm is licensed as:
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
;;;; The exported functions keep the names and lambda lists of CLX's
;;;; resource manager (name lists of strings or symbols, GET-RESOURCE with
;;;; a separate value name and class, search tables), but their results
;;;; are xcb-util-xrm's:
;;;;
;;;;   * A database is an ordered list of entries.  Adding an entry whose
;;;;     components equal an existing entry's replaces it, and the new
;;;;     entry goes to the end (__xcb_xrm_database_put).
;;;;   * The single-component wildcard ? is supported, as the string or
;;;;     symbol "?" in a name list.
;;;;   * A query is matched against every entry in order; the first match
;;;;     is the best until a later entry beats it under __match_compare's
;;;;     precedence rules, which are ported as they are.
;;;;   * Names compare case-sensitively (strcmp).  A symbol compares by its
;;;;     name; *UPPERCASE-RESOURCE-SYMBOLS* makes a comparison involving a
;;;;     symbol ignore case, which xcb-util-xrm, having no symbols, lacks.
;;;;
;;;; xcb-util-xrm finds a duplicate entry by comparing it with every entry;
;;;; here an index on the entries' components does that, with the same
;;;; result, so adding N entries is not quadratic.

(in-package #:xrm)

(declaim (inline %component-char-p %octal-digit-p))

(deftype stringable ()
  "A resource name component: a string (compared case-sensitively) or a
   symbol (see *UPPERCASE-RESOURCE-SYMBOLS*)."
  '(or string symbol))

(defun kintern (name)
  (intern (string name) :keyword))

(defparameter *uppercase-resource-symbols* nil
  "If true, a comparison involving a symbol ignores case, so a query
   with ordinary reader symbols such as :BACKGROUND matches the resource
   \"background\" and RESOURCE-KEY upcases a string before making it a keyword.
   NIL by default: then a symbol matches only by its exact name, :|background|,
   and strings are the way to query.  Strings always compare case-sensitively.")

(defun stringable-equal (a b)
  "Compare two stringables.

  Strings compare case-sensitively; a
  comparison involving a symbol ignores case when
  *UPPERCASE-RESOURCE-SYMBOLS* is true."
  (declare (type stringable a b))
  (if (and *uppercase-resource-symbols* (or (symbolp a) (symbolp b)))
      (string-equal (string a) (string b))
      (string= (string a) (string b))))

;;; entry.h

;;; TYPE is :NORMAL or :WILDCARD (CT_NORMAL, CT_WILDCARD); BINDING is
;;; :TIGHT or :LOOSE (BT_TIGHT, BT_LOOSE); NAME is a stringable, or NIL
;;; for a wildcard.
(defstruct (component (:constructor make-component (type binding name))
                      (:copier nil) (:predicate nil))
  (type :normal :type (member :normal :wildcard) :read-only t)
  (binding :tight :type (member :tight :loose) :read-only t)
  (name nil :type (or null stringable) :read-only t))

(defstruct (entry (:constructor make-entry (components value))
                  (:copier nil) (:predicate nil))
  (components '() :type list :read-only t)
  (value nil)
  (deleted nil))

(defun %entry-key (components)
  "An EQUAL key identifying COMPONENTS as __xcb_xrm_entry_compare does:
   type, binding and, for a normal component, the name.  Each name is
   written with its length before it, so no name, whatever characters
   it holds, can be mistaken for a component boundary."
  (with-output-to-string (out)
    (dolist (c components)
      (write-char (if (eq (component-type c) :normal) #\N #\W) out)
      (write-char (if (eq (component-binding c) :tight) #\T #\L) out)
      (when (eq (component-type c) :normal)
        (let ((name (string (component-name c))))
          (format out "~D:~A" (length name) name))))))

;;; The entry parser (xcb_xrm_entry_parse)

;; The characters allowed in a component name: - _ 0-9 a-z A-Z.
(defun %component-char-p (c)
  (or (char<= #\0 c #\9)
      (char<= #\a c #\z)
      (char<= #\A c #\Z)
      (char= c #\_)
      (char= c #\-)))

;; (defun %component-char-p (c)
;;   (declare (type character c))
;;   (find c #.(coerce "-_0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
;;                     'simple-vector)))

(defun %octal-digit-p (c)
  (and c (char<= #\0 c #\7)))

;; Parse STRING as xcb_xrm_entry_parse does.  Returns (VALUES COMPONENTS
;; VALUE) on success, or NIL if STRING is not a valid entry.  With
;; RESOURCE-ONLY, STRING must be a plain resource name (a query): only
;; tight bindings and normal components, and no value.
(defun %parse-entry (string &optional resource-only)
  (declare (type string string))
  (let ((chunk 0)           ; CS_INITIAL 0, CS_COMPONENTS 1,
                            ; CS_PRE_VALUE_WHITESPACE 2, CS_VALUE 3
        (binding :tight)    ; state.current_binding_type
        (buffer (make-string-output-stream))
        (buffer-empty t)
        (components '())
        (value (make-string-output-stream))
        (n (length string))
        (i 0))
    (labels ((finalize ()
               ;; xcb_xrm_finalize_component
               (unless buffer-empty
                 (push (make-component :normal binding
                                       (get-output-stream-string buffer))
                       components)
                 (setf buffer-empty t))
               (setf binding :tight))
             (ch (k) (and (< (+ i k) n)
                          (char string (+ i k))))
             (fail () (return-from %parse-entry nil)))
      (loop while (< i n)
            do (let ((c (char string i)))
                 (flet ((process-normally ()
                          (setf chunk (max chunk 1))
                          (when (= chunk 2)
                            (setf chunk 3))
                          (when (and (= chunk 1)
                                     (not (%component-char-p c)))
                            (fail))
                          (if (< chunk 3)
                              (progn
                                (write-char c buffer)
                                (setf buffer-empty nil))
                              (if (char= c #\\)
                                  (let ((c1 (ch 1)))
                                    (cond
                                      ((eql c1 #\Space)
                                       (write-char #\Space value)
                                       (incf i))
                                      ((eql c1 #\Tab)
                                       (write-char #\Tab value)
                                       (incf i))
                                      ((eql c1 #\\)
                                       (write-char #\\ value)
                                       (incf i))
                                      ((eql c1 #\n)
                                       (write-char #\Newline value)
                                       (incf i))
                                      ((and (%octal-digit-p c1)
                                            (%octal-digit-p (ch 2))
                                            (%octal-digit-p (ch 3)))
                                       (write-char
                                        ;; Truncated to 8 bits, as C's char
                                        ;; truncates \777.
                                        (code-char
                                         (logand
                                          (+ (* 64 (digit-char-p c1))
                                             (* 8 (digit-char-p (ch 2)))
                                             (digit-char-p (ch 3)))
                                          #xff))
                                        value)
                                       (incf i 3))
                                      (t (write-char c value))))
                                  (write-char c value)))))
                   (case c
                     ((#\. #\*)
                      (setf chunk (max chunk 1))
                      (if (>= chunk 2)
                          (process-normally)
                          (progn
                            (when (and (char= c #\*) resource-only)
                              (fail))
                            ;; A run of bindings collapses into one, loose
                            ;; if any of them is.
                            (let ((new (if (char= c #\*) :loose :tight)))
                              (loop while (member (ch 1) '(#\. #\*))
                                    do (incf i)
                                       (when (char= (char string i) #\*)
                                         (setf new :loose)))
                              (finalize)
                              (setf binding new)))))
                     (#\?
                      (setf chunk (max chunk 1))
                      (if (>= chunk 2)
                          (process-normally)
                          (progn
                            (when resource-only (fail))
                            ;; Unlike xcb-util-xrm, flush the pending name before
                            ;; inserting the wildcard, as '.' and '*' do; otherwise
                            ;; "abc?def" parses as (WILDCARD "abcdef") rather than
                            ;; ("abc" WILDCARD "def"). The binding is not reset by
                            ;; '?', so restore it after the finalize
                            ;; (which sets it to :tight).
                            (let ((b binding))
                              (finalize)
                              (push (make-component :wildcard b nil) components)
                              (setf binding b)))))
                     ((#\Space #\Tab)
                      ;; Whitespace is only kept in the value; whitespace
                      ;; between the ':' and the value is dropped.
                      (unless (<= chunk 2)
                        (process-normally)))
                     (#\:
                      (when resource-only
                        (fail))
                      (case chunk
                        (0 (fail))
                        (1 (finalize) (setf chunk 2))
                        (t (setf chunk 3) (process-normally))))
                     (otherwise
                      (process-normally))))
                 (incf i)))
      (cond ((or (= chunk 2) (= chunk 3)))
            ((not resource-only) (fail))
            (t (finalize)))
      (setf components (nreverse components))
      ;; An entry needs a component, and its last one must be a name.
      (when (or (null components)
                (not (eq (component-type (car (last components))) :normal)))
        (fail))
      (values components (if resource-only
                             nil
                             (get-output-stream-string value))))))

;; The text of an entry, as __xcb_xrm_entry_to_string writes it.
(defun %entry-string (components value &optional (escape #'identity))
  (with-output-to-string (out)
    (loop for c in components
          for first = t then nil
          do (unless (and first (eq (component-binding c) :tight))
               (write-char (if (eq (component-binding c) :tight)
                               #\.
                               #\*)
                           out))
             (write-string (if (eq (component-type c) :normal)
                               (string (component-name c))
                               "?")
                           out))
    (write-string ": " out)
    (write-string (funcall escape value) out)))

;;; Databases (database.c)

(defstruct (resource-database (:copier nil) (:predicate nil)
                              (:print-function print-resource-database)
                              (:constructor make-resource-database-internal))
  ;; The entries in order, with deleted ones marked (and compacted away
  ;; when they are many), so that replacing an entry is not linear.
  (entries (make-array 16 :adjustable t :fill-pointer 0))
  (deleted 0 :type fixnum)
  ;; %ENTRY-KEY -> live entry.
  (index (make-hash-table :test 'equal))
  (stamp 0 :type integer))

(defun print-resource-database (database stream depth)
  (declare (ignore depth))
  (print-unreadable-object (database stream :type t :identity t)
    (format stream "~D entr~:@P" (hash-table-count (resource-database-index database)))))

(defun make-resource-database ()
  "Return a new, empty resource database."
  (make-resource-database-internal))

(defun %incf-timestamp (database)
  (incf (resource-database-stamp database)))

(defmacro do-entries ((entry database) &body body)
  "Run BODY with ENTRY bound to each live entry of DATABASE, in order."
  (let ((v (gensym "ENTRIES")))
    `(let ((,v (resource-database-entries ,database)))
       (loop for ,entry across ,v
             unless (entry-deleted ,entry)
               do (progn ,@body)))))

(defun %compact (database)
  (let* ((old (resource-database-entries database))
         (new-size (max 16 (- (length old) (resource-database-deleted database))))
         (new (make-array new-size :adjustable t :fill-pointer 0)))
    (loop for e across old
          unless (entry-deleted e) do (vector-push-extend e new))
    (setf (resource-database-entries database) new
          (resource-database-deleted database) 0)))

(defun %remove-entry (database entry)
  (setf (entry-deleted entry) t)
  (remhash (%entry-key (entry-components entry)) (resource-database-index database))
  (when (> (incf (resource-database-deleted database))
           (max 64 (floor (fill-pointer (resource-database-entries database)) 2)))
    (%compact database)))

;; __xcb_xrm_database_put: add an entry, replacing an equal one if
;; OVERRIDE (the new entry goes to the end), or keeping the existing one
;; otherwise.
(defun %put (database components value &optional (override t))
  (let* ((key (%entry-key components))
         (current (gethash key (resource-database-index database))))
    (when (and current (not override))
      (return-from %put nil))
    (when current
      (%remove-entry database current))
    (let ((entry (make-entry components value)))
      (vector-push-extend entry (resource-database-entries database))
      (setf (gethash key (resource-database-index database)) entry)
      (%incf-timestamp database)
      entry)))

;; The components of NAME-LIST, a list of stringables in which * is a
;; loose binding before the next component and ? a wildcard component.
(defun %name-list-components (name-list)
  "The components of NAME-LIST, a list of stringables in which * is a
   loose binding before the next component and ? a wildcard component.
   A component is loose only if * directly precedes it, a ? included,
   so that every entry has exactly one name list (the one MAP-RESOURCE
   reports).  This differs on purpose from resource text, where, as in
   xcb-util-xrm, a name written right after ?, with no . or * between,
   keeps the ?'s binding: \"*?b\" is the name list (\"*\" \"?\" \"*\" \"b\"),
   and (\"*\" \"?\" \"b\") is \"*?.b\"."
  (let ((binding :tight)
        (components '()))
    (dolist (name name-list)
      (cond ((string= (string name) "*")
             (setf binding :loose))
            ((string= (string name) "?")
             (push (make-component :wildcard binding nil) components)
             (setf binding :tight))
            (t
             (push (make-component :normal binding name) components)
             (setf binding :tight))))
    (nreverse components)))

;; The name list of COMPONENTS, as MAP-RESOURCE reports it.
(defun %components-name-list (components)
  (loop for c in components
        when (eq (component-binding c) :loose) collect "*"
        collect (if (eq (component-type c) :normal) (component-name c) "?")))

(defun resource-database-timestamp (database)
  "Return DATABASE's modification counter, incremented by every
   ADD-RESOURCE and DELETE-RESOURCE."
  (declare (type resource-database database))
  (resource-database-stamp database))

(defun add-resource (database name-list value)
  "Add VALUE to DATABASE under NAME-LIST, a list of strings or symbols.
   The symbol or string * is a loose binding before the next component,
   and ? is a component matching any single name.  Only a * directly
   before a component makes it loose, a ? included: (\"*\" \"?\" \"b\")
   is the resource \"*?.b\", and \"*?b\" in resource text, where b
   keeps the ?'s loose binding, is (\"*\" \"?\" \"*\" \"b\").

   VALUE must not be NIL.  As in xcb-util-xrm, an existing resource with the same name
   list is replaced, and the new one goes to the end of the database.
   Signals an error for a name list that names no resource or ends in
   a wildcard."
  (declare (type resource-database database)
           (type list name-list))
  (unless value (error "Null resource values are ignored"))
  (let ((components (%name-list-components name-list)))
    (when (or (null components)
              (not (eq (component-type (car (last components))) :normal)))
      (error "Resource name list ~S names no resource." name-list))
    (%put database components value)
    value))

(defun delete-resource (database name-list)
  "Remove the resource stored under NAME-LIST from DATABASE.  Returns T
   if it existed."
  (declare (type resource-database database)
           (type list name-list))
  (let ((entry (gethash (%entry-key (%name-list-components name-list))
                        (resource-database-index database))))
    (%incf-timestamp database)
    (when entry
      (%remove-entry database entry)
      t)))

;;; Matching (match.c)

;;; xcb_xrm_match_flags_t
(defconstant +mf-none+             0)
(defconstant +mf-name+             (ash 1 1))
(defconstant +mf-class+            (ash 1 2))
(defconstant +mf-wildcard+         (ash 1 3))
(defconstant +mf-skipped+          (ash 1 4))
(defconstant +mf-preceding-loose+  (ash 1 5))
(defconstant +mf-matched+ (logior +mf-name+ +mf-class+ +mf-wildcard+))

;; __match_matches_component.
(defun %match-component (db name class)
  (let ((result (if (eq (component-binding db) :loose)
                    +mf-preceding-loose+
                    +mf-none+)))
    (if (eq (component-type db) :normal)
        (cond
          ((stringable-equal (component-name db) name)
           (logior result +mf-name+))
          ((and class (stringable-equal (component-name db) class))
           (logior result +mf-class+))
          ((eq (component-binding db) :tight)
           +mf-none+)
          (t
           ;; The loose flag goes on the last component of the match
           ;; for the loose binding, not on skipped ones.
           (logior (logandc2 result +mf-preceding-loose+) +mf-skipped+)))
        (logior result +mf-wildcard+))))

(defun %matches (db names classes has-class position ignore flags)
  (when (or (null names) (and has-class (null classes)) (null db))
    (return-from %matches
      (values (and (null db) (null names) (or (not has-class) (null classes)))
              flags)))
  (let ((match (%match-component
                (car db) (car names) (and has-class (car classes)))))
    ;; A matching component in a loose binding: try matching it, and
    ;; then try skipping it.
    (when (and (eq ignore :undecided)
               (eq (component-binding (car db)) :loose)
               (logtest match +mf-matched+))
      ;; A matching component in a loose binding: try matching it first, then
      ;; try skipping it. No save/restore of FLAGS is needed: on success the
      ;; recursion from this call writes every flag position POSITION..N-1,
      ;; so the second recursive call rewrites anything the first may have
      ;; left behind before failing; a failed call's flags are never read.
      (multiple-value-bind (ok result)
          (%matches db names classes has-class position :do-not-ignore flags)
        (when ok
          (return-from %matches (values t result))))
      (return-from %matches
        (%matches db names classes has-class position :ignore flags)))
    (setf (aref flags position) match)
    (when (= match +mf-none+)
      (return-from %matches (values nil flags)))
    (%matches (if (and (not (eq ignore :ignore)) (logtest match +mf-matched+))
                  (cdr db)
                  db)
              (cdr names)
              (and has-class (cdr classes))
              has-class (1+ position) :undecided flags)))

;; __match_compare: is CANDIDATE a better match than BEST?  Both are
;; flag vectors.  Ported as it is: it never decides for BEST early.
(defun %betterp (best candidate)
  "__match_compare: is CANDIDATE a better match than BEST?  Both are
   flag vectors.  Ported as it is: it never decides for BEST early."
  (declare (type (vector fixnum) best candidate))
  (loop for b of-type fixnum across best
        for c of-type fixnum across candidate
        do (cond
             ;; Rule 1: matching components, including ?, outweigh *.
             ((and (logtest b +mf-skipped+) (logtest c +mf-matched+))
              (return t))
             ;; Rule 2: a name outweighs a class and ?; a class outweighs ?.
             ((and (logtest b (logior +mf-class+ +mf-wildcard+)) (logtest c +mf-name+))
              (return t))
             ((and (logtest b +mf-wildcard+) (logtest c +mf-class+))
              (return t))
             ;; Rule 3: a preceding exact match outweighs a preceding *.
             ((and (logtest b +mf-preceding-loose+) (not (logtest c +mf-preceding-loose+)))
              (return t)))))

;; __xcb_xrm_match: the value of the entry of DATABASE that best matches
;; the query NAMES / CLASSES (CLASSES may be NIL), as
;; (VALUES VALUE FOUNDP).
(defun %match (database names classes)
  (let ((num (length names))
        (has-class (not (null classes)))
        (best nil)
        (best-flags nil))
    (do-entries (entry database)
      (multiple-value-bind (ok flags)
          (%matches (entry-components entry) names classes has-class 0 :undecided
                    (make-array num :element-type 'fixnum :initial-element 0))
        (when (and ok (or (null best) (%betterp best-flags flags)))
          (setf best entry
                best-flags flags))))
    (if best
        (values (entry-value best) t)
        (values nil nil))))

(defun %valid-query-name-p (name)
  (let ((s (string name)))
    (and (plusp (length s)) (every #'%component-char-p s))))

;; __resource_get: match NAMES and CLASSES, lists of stringables.  As in
;; xcb-util-xrm, a query name must be a plain resource name, and CLASSES,
;; unless NIL, must have as many components as NAMES.
(defun %query (database names classes)
  (if (or (null names)
          (notevery #'%valid-query-name-p names)
          (and classes
               (or (notevery #'%valid-query-name-p classes)
                   (/= (length names) (length classes)))))
      (values nil nil)
      (%match database names classes)))

;;; Get resource

(defun get-resource (database value-name value-class full-name full-class)
  "Return the value of the resource in DATABASE that best matches the
   name (APPEND FULL-NAME (LIST VALUE-NAME)) and class
   (APPEND FULL-CLASS (LIST VALUE-CLASS)), as xcb-util-xrm's
   xcb_xrm_resource_get_string does.  FULL-NAME and FULL-CLASS are lists
   of stringables, e.g. (\"xclock\" \"clock\") and (\"XClock\" \"Clock\").
   A NIL VALUE-CLASS with a NIL FULL-CLASS queries by name only.  As
   in xcb-util-xrm, a query is a plain name: a component that is * or
   ?, or contains anything but A-Z a-z 0-9 _ -, matches nothing.  So
   the name lists here are not ADD-RESOURCE's: they have no bindings
   or wildcards, and the rule there for which component a * or ?
   applies to does not come into it.  A \"*\" or \"?\" in FULL-NAME
   or FULL-CLASS, as in a name list copied from MAP-RESOURCE, makes
   the query match nothing rather than act as a pattern.
   Returns (VALUES VALUE FOUNDP); VALUE is NIL when nothing matches."
  (declare (type resource-database database)
           (type list full-name full-class))
  (%query database
          (append full-name (list value-name))
          (if (and (null value-class) (null full-class))
              nil
              (append full-class (list value-class)))))

;; Parse STRING as xcb-util-xrm's str2long does with base 10: an
;; optionally signed decimal integer and nothing else; no whitespace
;; before or after it.  Returns the integer or NIL (C's long range
;; limit does not apply).  PARSE-INTEGER is not enough on its own:
;; per the spec it permits leading and trailing whitespace around the
;; integer, which str2long's isspace()/`*end` checks reject.
(defun %parse-decimal (string)
  (and (stringp string)
       (plusp (length string))
       (notany (lambda (c) (member c '(#\Space #\Tab #\Newline #\Return #\Page)))
               (list (char string 0) (char string (1- (length string)))))
       (ignore-errors (parse-integer string))))

(defun get-resource-integer (database value-name value-class full-name full-class)
  "GET-RESOURCE, converting the value to an integer, as xcb-util-xrm's
   xcb_xrm_resource_get_long does: an optionally signed decimal integer,
   with nothing around it.  Returns (VALUES INTEGER CONVERTED-P RAW):
   INTEGER is NIL unless CONVERTED-P, and RAW is the resource's string
   when there is one, so a missing resource (RAW NIL) and one that is
   not a number (RAW a string) can be told apart, as the C function's
   -2 and -1 results do."
  (let* ((raw (get-resource database value-name value-class full-name full-class))
         (n (%parse-decimal raw)))
    (values n (and n t) raw)))

(defun get-resource-boolean (database value-name value-class full-name full-class)
  "GET-RESOURCE, converting the value to a boolean, as xcb-util-xrm's
   xcb_xrm_resource_get_bool does: an integer (as for
   GET-RESOURCE-INTEGER) is T unless it is 0; otherwise true, on and yes
   are T and false, off and no are NIL, case-insensitively.  Returns
   (VALUES BOOLEAN CONVERTED-P RAW), as GET-RESOURCE-INTEGER."
  (let* ((raw (get-resource database value-name value-class full-name full-class))
         (n (%parse-decimal raw)))
    (cond
      ((not (stringp raw)) (values nil nil raw))
      (n (values (/= n 0) t raw))
      ((member raw '("true" "on" "yes") :test #'string-equal) (values t t raw))
      ((member raw '("false" "off" "no") :test #'string-equal) (values nil t raw))
      (t (values nil nil raw)))))

;;; Search tables

;;; xcb-util-xrm has no search tables.  A search table is the database
;;; and the query prefix, so that GET-SEARCH-RESOURCE is exactly
;;; GET-RESOURCE.

(defstruct (search-table (:constructor make-search-table (database full-name full-class))
                         (:copier nil) (:predicate nil))
  (database nil :read-only t)
  (full-name nil :read-only t)
  (full-class nil :read-only t))

(defun get-search-table (database full-name full-class)
  "Return a search table of DATABASE for FULL-NAME and FULL-CLASS, for
   use with GET-SEARCH-RESOURCE."
  (declare (type resource-database database)
           (type list full-name full-class))
  (make-search-table database (copy-list full-name) (copy-list full-class)))

(defun get-search-resource (table name class)
  "Look NAME / CLASS up in TABLE, a search table from GET-SEARCH-TABLE:

       (get-search-resource (get-search-table db full-name full-class)
                            value-name value-class)

   is (get-resource db value-name value-class full-name full-class)."
  (values (get-resource (search-table-database table) name class
                        (search-table-full-name table)
                        (search-table-full-class table))))

;;; Utility functions

(defun map-resource (database function &rest args)
  "Call FUNCTION on each resource in DATABASE, with arguments
   (NAME-LIST VALUE . ARGS), in database order.  In NAME-LIST a loose
   binding is the string \"*\" before its component, and a wildcard the
   string \"?\"."
  (declare (type resource-database database)
           (type function function))
  (let ((entries '()))
    ;; Collect first, so FUNCTION may modify DATABASE.
    (do-entries (entry database)
      (push entry entries))
    (dolist (entry (nreverse entries))
      (apply function (%components-name-list (entry-components entry))
             (entry-value entry) args))))

(defun merge-resources (database with-database)
  "Add every resource of DATABASE to WITH-DATABASE, overriding
   WITH-DATABASE's entries of the same name, as xcb-util-xrm's
   xcb_xrm_database_combine does with override.  Returns WITH-DATABASE."
  (declare (type resource-database database with-database))
  (unless (eq database with-database)
    (let ((entries '()))
      (do-entries (entry database)
        (push entry entries))
      (dolist (entry (nreverse entries))
        (%put with-database (entry-components entry) (entry-value entry)))))
  with-database)

;; CLX's RESOURCE-KEY, for callers that build keyword name lists; clXrm
;; itself has no use for it.
(defun resource-key (stringable)
  "Return STRINGABLE as a keyword, upcasing a string first when
   *UPPERCASE-RESOURCE-SYMBOLS* is true.  As in CLX, which exports it
   for building symbol name lists; clXrm itself has no use for it."
  (check-type stringable xrm:stringable)
  (etypecase stringable
    (symbol
     (if (keywordp stringable)
         stringable
         (intern (symbol-name stringable) :keyword)))
    (string
     (intern (if xrm:*uppercase-resource-symbols*
                 (string-upcase stringable)
                 stringable)
             :keyword))))
