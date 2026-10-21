;;;; tests.lisp — clXrm tests; RT-based, no X server, no fixture files
;;;; Copyright (C) 2026 Arthur Miller
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; (asdf:test-system "clxrm") runs these.  On SBCL the ASDF system
;;;; loads SBCL's bundled sb-rt; elsewhere it must find the RT system.
;;;; Every fixture is built inline, so no data files are needed.

(defpackage #:clxrm/tests
  (:use #:cl #+sbcl #:sb-rt #-sbcl #:rt)
  (:shadow #:deftest)
  (:export #:run-tests))

(in-package #:clxrm/tests)

;;;--------------------------------------------------------------------
;;; Test definition on top of RT

;;; The tests are written as (DEFTEST name () body...) with IS and
;;; IS-EQUAL assertions in the body.  RT's own DEFTEST takes a single
;;; form and its expected values instead, so this DEFTEST wraps the body
;;; into an RT test whose expected value is T: the test passes when no
;;; assertion failed, and otherwise returns the failed assertions, which
;;; RT then prints.

(defvar *failures* '()
  "The assertions that failed in the test being run.")

(defmacro is (form)
  "Assert that FORM is true."
  `(unless ,form
     (push '(:failed ,form) *failures*)))

(defmacro is-true (form)
  "Assert that FORM is true; the same as IS."
  `(is ,form))

(defmacro is-false (form)
  "Assert that FORM is false."
  `(is (not ,form)))

(defmacro is-equal (got want)
  "Assert that GOT is EQUAL to WANT."
  (let ((g (gensym "GOT")) (w (gensym "WANT")))
    `(let ((,g ,got) (,w ,want))
       (unless (equal ,g ,w)
         (push (list :form ',got :got ,g :want ,w) *failures*)))))

(defmacro deftest (name lambda-list &body body)
  "Define an RT test NAME that runs BODY and passes if no IS or IS-EQUAL
   in it failed.  LAMBDA-LIST must be empty."
  (assert (null lambda-list) () "DEFTEST ~S takes no arguments." name)
  `(#+sbcl sb-rt:deftest #-sbcl rt:deftest ,name
     (let ((*failures* '()))
       ,@body
       (or (null *failures*) (reverse *failures*)))
     t))

;;; Helpers

(defmacro check (form &optional expected (test '#'equal))
  "Run FORM.  Without EXPECTED, require a true result.  With EXPECTED,
   compare to it using TEST, EQUAL by default.  Errors become the
   result (:ERROR message), so a test can assert that something
   signals, with ERROR-RESULT-P as TEST."
  (if expected
      `(let ((got (handler-case ,form
                    (error (e) (list :error (princ-to-string e)))))
             (want ,expected))
         (is (funcall ,test got want)))
      `(is (handler-case (progn ,form t) (error () nil)))))

(defun error-result-p (got want)
  "For CHECK: GOT is the (:ERROR message) that CHECK makes of a
   signalled error."
  (and (consp got) (eq (first got) want)))

(defun db-from (text)
  "A database read from TEXT, after FORMAT has processed it, so that
   test fixtures can write ~% for a newline."
  (xrm:read-resources-from-string (xrm:make-resource-database)
                                  (format nil text)))

(defun octets (&rest bytes)
  "A SIMPLE-ARRAY (UNSIGNED-BYTE 8) (*) with BYTES."
  (coerce bytes '(simple-array (unsigned-byte 8) (*))))

(defun lookup (db name class &optional (names '("app")) (classes '("App")))
  (xrm:get-resource db name class names classes))

(defun sorted-lines (string)
  (sort (remove "" (uiop:split-string string :separator '(#\Newline))
                :test #'string=)
        #'string<))

(defun symbolise (x)
  (cond ((stringp x) (intern x :keyword))
        ((consp x) (mapcar #'symbolise x))
        (t x)))

(defun line (name value)
  "One line of WRITE-RESOURCES output: name, \": \", value, as
   xcb_xrm_database_to_string writes it."
  (format nil "~A: ~A" name value))

;;; The resource file the CLX reference tests read.  This is the old
;;; suite's fixture.xrdb, as a string.  The expected values are what
;;; CLX 0.7.8's resource.lisp answers for it when queried with symbols
;;; (CLX signals a TYPE-ERROR for string names on SBCL); clXrm must
;;; give the same answers for both.  Whitespace around the colon is
;;; not significant, so spaces stand in for the file's tabs.

(defparameter *clx-fixture*
  "*Background: grey
*Font: helvetica
*font: fixed
Emacs*Foreground: black
Included.resource: from-include
XClock*Label.font: times
XClock*clock.height: 200
XClock.background: MidnightBlue
XClock.update: 5
emacs.default.attributeForeground: white
xclock*hands: red
xclock*padding: 4
xclock.clock.width: 300
xclock.foreground: PapayaWhip
xterm*vt100.geometry: 80x24
xterm.vt100*background: black
")

(defparameter *clx-get-resource*
  '((("background" "Background" ("xclock") ("XClock")) "MidnightBlue")
    (("foreground" "Foreground" ("xclock") ("XClock")) "PapayaWhip")
    (("hands" "Hands" ("xclock") ("XClock")) "red")
    (("font" "Font" ("xclock") ("XClock")) "fixed")
    (("font" "Font" ("xclock" "label") ("XClock" "Label")) "times")
    (("width" "Width" ("xclock" "clock") ("XClock" "Clock")) "300")
    (("height" "Height" ("xclock" "clock") ("XClock" "Clock")) "200")
    (("padding" "Padding" ("xclock" "clock") ("XClock" "Clock")) "4")
    (("padding" "Padding" ("xclock") ("XClock")) "4")
    (("foreground" "Foreground" ("emacs") ("Emacs")) "black")
    (("attributeForeground" "AttributeForeground"
      ("emacs" "default") ("Emacs" "Face"))
     "white")
    (("background" "Background" ("emacs") ("Emacs")) "grey")
    (("geometry" "Geometry" ("xterm" "vt100") ("XTerm" "VT100")) "80x24")
    (("background" "Background" ("xterm" "vt100") ("XTerm" "VT100")) "black")
    (("resource" "Resource" ("included") ("Included")) "from-include")
    (("update" "Update" ("xclock") ("XClock")) "5")
    (("nothing" "Nothing" ("nope") ("Nope")) nil)))

(defparameter *clx-search*
  '((("width" "Width") "300")
    (("height" "Height") "200")
    (("padding" "Padding") "4")
    (("font" "Font") "fixed")
    (("background" "Background") "grey")))

(defparameter *clx-written*
  (sort (list (line "*Background" "grey")
              (line "*Font" "helvetica")
              (line "*font" "fixed")
              (line "Emacs*Foreground" "black")
              (line "Included.resource" "from-include")
              (line "XClock*Label.font" "times")
              (line "XClock*clock.height" "200")
              (line "XClock.background" "MidnightBlue")
              (line "XClock.update" "5")
              (line "emacs.default.attributeForeground" "white")
              (line "xclock*hands" "red")
              (line "xclock*padding" "4")
              (line "xclock.clock.width" "300")
              (line "xclock.foreground" "PapayaWhip")
              (line "xterm*vt100.geometry" "80x24")
              (line "xterm.vt100*background" "black"))
        #'string<))

;;; The CLX reference: matching, search table, writing, merge, delete

(deftest test-clx-reference-get-resource ()
  (let ((db (db-from *clx-fixture*)))
    (dolist (case *clx-get-resource*)
      (destructuring-bind (query want) case
        (is-equal (apply #'xrm:get-resource db query) want)
        (is-equal (apply #'xrm:get-resource db (symbolise query)) want)))))

(deftest test-clx-reference-found-p ()
  (let ((db (db-from *clx-fixture*)))
    (is-true (nth-value 1 (xrm:get-resource db "background" "Background"
                                              '("xclock") '("XClock"))))
    (is-false (nth-value 1 (xrm:get-resource db "nothing" "Nothing"
                                               '("nope") '("Nope"))))))

(deftest test-clx-reference-search-table ()
  (let* ((db (db-from *clx-fixture*))
         (table (xrm:get-search-table db '("xclock" "clock") '("XClock" "Clock"))))
    (dolist (case *clx-search*)
      (is-equal (apply #'xrm:get-search-resource table (first case))
                (second case)))))

(deftest test-clx-reference-write ()
  (is-equal (sorted-lines (xrm:write-resources-to-string (db-from *clx-fixture*)))
            *clx-written*))

(deftest test-clx-reference-key-test ()
  (let ((db (xrm:read-resources-from-string
             (xrm:make-resource-database) *clx-fixture*
             :key #'string-upcase
             :test (lambda (names value)
                     (declare (ignore value))
                     (member "xclock" names :test #'equal)))))
    (is-equal (sorted-lines (xrm:write-resources-to-string db))
              (sort (list (line "xclock*hands" "RED")
                          (line "xclock*padding" "4")
                          (line "xclock.clock.width" "300")
                          (line "xclock.foreground" "PAPAYAWHIP"))
                    #'string<))))

(deftest test-clx-reference-merge-and-delete ()
  (let ((db (db-from *clx-fixture*))
        (db3 (xrm:make-resource-database)))
    (xrm:add-resource db3 '("xclock" "background") "green")
    (xrm:add-resource db3 '("xclock" * "extra") "yes")
    (xrm:merge-resources db db3)
    (is-equal (lookup db3 "background" "Background" '("xclock") '("XClock"))
              "green")
    (is-equal (lookup db3 "extra" "Extra" '("xclock" "a" "b") '("XClock" "A" "B"))
              "yes")
    (is-true (xrm:delete-resource db3 '("xclock" "background")))
    (is-equal (lookup db3 "background" "Background" '("xclock") '("XClock"))
              "MidnightBlue")
    (is-false (xrm:delete-resource db3 '("xclock" "background")))))

(deftest test-add-resource-rejects-nil ()
  (is-true (handler-case
               (progn (xrm:add-resource (xrm:make-resource-database) '("a") nil)
                      nil)
             (error () t))))

;;; Matching details beyond the CLX reference

(deftest test-matching-string-and-symbol ()
  (let ((db (db-from "xclock.background: navy~%")))
    (dolist (c '(("background" "Background" ("xclock") ("XClock"))))
      (is-equal (apply #'xrm:get-resource db c)
                (apply #'xrm:get-resource db (symbolise c))))))

(deftest test-matching-class-vs-name ()
  ;; *UPPERCASE-RESOURCE-SYMBOLS* is the only thing that makes a symbol
  ;; match a resource whose string differs in case.
  (let ((db (db-from "xclock.background: navy~%")))
    (let ((xrm:*uppercase-resource-symbols* nil))
      (is-equal (lookup db "background" "Background" '("xclock") '("XClock"))
                "navy")
      (is-equal (lookup db :|background| :|Background| '(:|xclock|) '(:|XClock|))
                "navy")
      (is-equal (lookup db :background :background '(:xclock) '(:xclock)) nil))
    (let ((xrm:*uppercase-resource-symbols* t))
      (is-equal (lookup db "background" "Background" '("xclock") '("XClock"))
                "navy")
      (is-equal (lookup db :background :background '(:xclock) '(:xclock))
                "navy")
      ;; Strings stay case-sensitive even then.
      (is-equal (lookup db "BACKGROUND" "BACKGROUND" '("XCLOCK") '("XCLOCK"))
                nil))))

(deftest test-question-mark-is-wildcard ()
  ;; As in xcb-util-xrm, ? matches exactly one component.
  (let ((db (db-from "app.?.width: 100~%")))
    (is-equal (lookup db "width" "Width" '("app" "x") '("App" "X")) "100")
    (is-equal (lookup db "width" "Width" '("app") '("App")) nil)
    (is-equal (lookup db "width" "Width" '("app" "x" "y") '("App" "X" "Y")) nil)
    ;; A query name cannot contain ?.
    (is-equal (lookup db "width" "Width" '("app" "?") '("App" "Q")) nil)))

(deftest test-wildcard-does-not-reset-binding ()
  ;; xcb-util-xrm keeps the binding after ?, so in *?b the b is loose.
  (let ((db (db-from "*?b: 1~%")))
    (is-equal (xrm:write-resources-to-string db) (format nil "*?*b: 1~%"))
    (is-equal (lookup db "b" "B" '("x" "y") '("X" "Y")) "1")))

(deftest test-entry-key-no-collision ()
  ;; A name containing the key's own separator characters is still a
  ;; different entry from the components it looks like.
  (let ((db (xrm:make-resource-database)))
    (xrm:add-resource db '("a|NTb") "one")
    (xrm:add-resource db '("a" "b") "two")
    (xrm:add-resource db '("1:ab") "three")
    (xrm:add-resource db '("ab") "four")
    (is-equal (length (sorted-lines (xrm:write-resources-to-string db))) 4)))

(deftest test-get-resource-rejects-patterns ()
  ;; Query names are plain names; * and ? in them match nothing.
  (let ((db (db-from "*b: 1~%app.?.c: 2~%")))
    (is-equal (lookup db "b" "B" '("*") '("*")) nil)
    (is-equal (lookup db "c" "C" '("app" "?") '("App" "?")) nil)
    (is-equal (lookup db "c" "C" '("app" "x") '("App" "X")) "2")))

(deftest test-name-list-wildcard-binding ()
  ;; In a name list only a directly preceding * makes a component
  ;; loose, so every entry has one name list, and the text forms map
  ;; to them as documented in ADD-RESOURCE.
  (let ((db (xrm:make-resource-database)))
    (xrm:add-resource db '("*" "?" "b") "tight")
    (xrm:add-resource db '("*" "?" "*" "c") "loose")
    (is-equal (xrm:write-resources-to-string db)
              (format nil "*?.b: tight~%*?*c: loose~%")))
  (let ((names '()))
    (xrm:map-resource
     (db-from "*?b: 1~%*?.c: 2~%")
     (lambda (name-list value) (push (list name-list value) names)))
    (is-equal (reverse names) '((("*" "?" "*" "b") "1") (("*" "?" "c") "2")))))

;;; Structure: order, replace, value-and-children, delete, index

(deftest test-add-replace-moves-to-end ()
  (let ((db (db-from (format nil "a.x: 1~%a.y: 2~%a.x: 3~%"))))
    ;; As in xcb-util-xrm, the replacing entry goes to the end.
    (is-equal (xrm:write-resources-to-string db)
              (format nil "a.y: 2~%a.x: 3~%"))))

(deftest test-delete-preserves-siblings ()
  (let ((db (db-from (format nil "a: 1~%a.b: 2~%x.y: 3~%x.y.z: 4~%"))))
    (is-true (xrm:delete-resource db '("a" "b")))
    (is-equal (lookup db "a" "A" '() '()) "1")          ; parent kept
    (is-equal (lookup db "b" "B" '("a") '("A")) nil)    ; child gone
    (is-true (xrm:delete-resource db '("x" "y")))
    (is-equal (lookup db "y" "Y" '("x") '("X")) nil)    ; value gone
    (is-equal (lookup db "z" "Z" '("x" "y") '("X" "Y")) "4") ; grandchild kept
    (is-false (xrm:delete-resource db '("x" "y")))      ; already gone
    (is-false (xrm:delete-resource db '("no" "such")))))

(deftest test-value-and-children-both-visited ()
  (let ((db (db-from (format nil "a.b: 1~%a.b.c: 2~%"))))
    (is-equal (xrm:write-resources-to-string db)
              (format nil "a.b: 1~%a.b.c: 2~%"))
    (let ((merged (xrm:merge-resources db (xrm:make-resource-database))))
      (is-equal (list (lookup merged "b" "B" '("a") '("A"))
                      (lookup merged "c" "C" '("a" "b") '("A" "B")))
                '("1" "2")))))

(deftest test-insertion-order-across-tight-and-loose ()
  (let ((text (format nil "b.x: 1~%*c: 2~%a.y: 3~%b*z: 4~%")))
    (is-equal (xrm:write-resources-to-string (db-from text)) text)))

(deftest test-timestamp-monotone ()
  (let* ((db (xrm:make-resource-database))
         (t0 (xrm:resource-database-timestamp db)))
    (xrm:add-resource db '("a") "1")
    (let ((t1 (xrm:resource-database-timestamp db)))
      (is-true (> t1 t0))
      (xrm:delete-resource db '("a"))
      (is-true (> (xrm:resource-database-timestamp db) t1)))))

(deftest test-indexed-siblings ()
  ;; Past the index threshold, lookups use a hash index; deletes must
  ;; maintain it; the case-insensitive fallback must still work.
  (let ((db (xrm:make-resource-database)))
    (dotimes (i 500)
      (xrm:add-resource db (list "app" (format nil "r~D" i))
                           (format nil "v~D" i)))
    (is-true (loop for i below 500
                   always (equal (lookup db (format nil "r~D" i) "R")
                                 (format nil "v~D" i))))
    (dotimes (i 250)
      (xrm:delete-resource db (list "app" (format nil "r~D" (* 2 i)))))
    (is-equal (list (lookup db "r10" "R") (lookup db "r11" "R"))
              '(nil "v11"))
    (let ((xrm:*uppercase-resource-symbols* t))
      (xrm:add-resource db '(:app :r11) "upcased")
      ;; :APP :R11 is a different entry from "app" "r11" that matches
      ;; the query equally well, so the earlier entry stays the best.
      (is-equal (lookup db "r11" "R") "v11"))))

(deftest test-large-file-fast ()
  (let ((text (with-output-to-string (out)
                (dotimes (i 5000) (format out "app.res~D: ~D~%" i i))))
        (start (get-internal-real-time)))
    (let ((db (db-from text)))
      (is-equal (lookup db "res4999" "Res") "4999")
      (is-true (< (- (get-internal-real-time) start)
                  internal-time-units-per-second)))))

(deftest test-empty-name-list ()
  (is-true (handler-case
               (progn (xrm:add-resource (xrm:make-resource-database) '(*) "v")
                      nil)
             (error () t))))

;;; Xlib file syntax: escapes, continuations, whitespace, includes

(deftest test-read-escapes ()
  (let ((db (db-from (format nil
                             "app.a: one \\~%two~%~
                              app.b: line1\\nline2~%~
                              app.c: back\\\\slash~%~
                              app.d: \\101\\102C~%~
                              app.e: \\  leading~%~
                              app.f   :   spaced colon~%~
                              app.g: other\\qescape~%"))))
    (is-equal (lookup db "a" "A") "one two")
    (is-equal (lookup db "b" "B") (format nil "line1~%line2"))
    (is-equal (lookup db "c" "C") "back\\slash")
    (is-equal (lookup db "d" "D") "ABC")
    (is-equal (lookup db "e" "E") "  leading")
    (is-equal (lookup db "f" "F") "spaced colon")
    (is-equal (lookup db "g" "G") "other\\qescape")))

(deftest test-read-short-octal-escapes ()
  ;; An octal escape needs exactly three digits, as in xcb-util-xrm
  ;; (entry.c) and libX11 (Xrm.c, GetDatabase: "If we found three digits
  ;; then insert that octal code").  With fewer, the backslash and the
  ;; digits are kept, as xcb-util-xrm does; libX11 would drop the
  ;; backslash.
  (let ((db (db-from (format nil "app.a: \\1~%app.b: \\12~%app.c: \\101~%"))))
    (is-equal (lookup db "a" "A") "\\1")
    (is-equal (lookup db "b" "B") "\\12")
    (is-equal (lookup db "c" "C") "A")))

(deftest test-read-trailing-whitespace ()
  (let ((db (db-from (format nil "app.t: abc   ~%~
                                  app.u: abc\\ ~%~
                                  app.w: abc\\\\ ~%"))))
    ;; xcb-util-xrm keeps trailing whitespace.
    (is-equal (lookup db "t" "T") "abc   ")
    (is-equal (lookup db "u" "U") "abc ")
    (is-equal (lookup db "w" "W") "abc\\ ")))

(deftest test-read-missing-colon ()
  ;; A line with no colon is malformed and should not add anything.
  (let ((db (db-from "app.a~%")))
    (is-equal (lookup db "a" "A") nil)
    (is-equal (lookup db "app" "App" nil nil) nil)))

(deftest test-write-round-trip ()
  (let ((db (xrm:make-resource-database))
        (tricky (format nil " a\\b~%c")))
    (xrm:add-resource db '("app" "v") tricky)
    (is-equal (lookup (db-from (xrm:write-resources-to-string db)) "v" "V")
              tricky)))

(deftest test-write-round-trip-whitespace ()
  (dolist (value (list (format nil "a~Cb" #\Tab) "abc " "abc  "
                       (format nil "x~C" #\Tab) " " "  both  "
                       (format nil "~Cmid~Cdle~C" #\Tab #\Tab #\Tab)))
    (let ((db (xrm:make-resource-database)))
      (xrm:add-resource db '("app" "v") value)
      (is-equal (lookup (db-from (xrm:write-resources-to-string db)) "v" "V")
                value))))

(deftest test-write-format ()
  (let ((db (xrm:make-resource-database)))
    (xrm:add-resource db '("app" "x") "1")
    (xrm:add-resource db '("*" "y") (format nil " a~%b\\ "))
    (is-equal (xrm:write-resources-to-string db)
              (format nil "app.x: 1~%*y: \\ a\\nb\\\\ ~%"))))

(deftest test-include-from-file ()
  ;; The only tests that need a real file: #include's path resolution
  ;; is relative to the including file, which a string stream lacks.
  ;; The files live in a fresh temporary directory made by the test.
  (let ((dir (uiop:ensure-directory-pathname
              (merge-pathnames (format nil "clxrm-rt-~D/" (random 1000000))
                               (uiop:temporary-directory)))))
    (ensure-directories-exist dir)
    (unwind-protect
         (progn
           (with-open-file (out (merge-pathnames "included.xrdb" dir)
                                :direction :output :if-exists :supersede)
             (write-string "app.included: yes" out))
           (with-open-file (out (merge-pathnames "main.xrdb" dir)
                                :direction :output :if-exists :supersede)
             (format out "#include \"included.xrdb\"~%app.main: also~%"))
           (let ((db (xrm:read-resources
                      (xrm:make-resource-database)
                      (merge-pathnames "main.xrdb" dir))))
             (is-equal (lookup db "included" "Included") "yes")
             (is-equal (lookup db "main" "Main") "also")))
      (uiop:delete-directory-tree dir :validate t :if-does-not-exist :ignore))))

(deftest test-include-loop-is-stopped ()
  (uiop:with-temporary-file (:pathname p :type "xrdb")
    (with-open-file (out p :direction :output :if-exists :supersede)
      (format out "#include \"~A\"~%" (file-namestring p))
      (format out "a: 1~%"))
    ;; xcb-util-xrm stops following includes 100 deep, silently.
    (is-equal (lookup (xrm:read-resources (xrm:make-resource-database) p)
                      "a" "A" '() '())
              "1")))

(deftest test-string-argument-is-a-pathname ()
  ;; As in CLX: a string argument to READ-RESOURCES names a file.
  (uiop:with-temporary-file (:pathname p :type "xrdb")
    (with-open-file (out p :direction :output :if-exists :supersede)
      (write-string "app.x: y" out))
    (is-equal (lookup (xrm:read-resources (xrm:make-resource-database)
                                            (namestring p)) "x" "X")
              "y")))

;;; put-resource / put-resource-line and typed getters

(deftest test-put-resource-line ()
  (let ((db (xrm:make-resource-database)))
    (is-equal (xrm:put-resource-line db "XClock.background:  navy") "navy")
    (is-equal (lookup db "background" "Background" '("xclock") '("XClock"))
              "navy")
    (xrm:put-resource-line db "app.v: a\\nb")
    (is-equal (lookup db "v" "V") (format nil "a~%b"))
    (is-equal (xrm:put-resource-line db "! comment") nil)
    (is-equal (xrm:put-resource-line db "   ") nil)
    ;; Invalid lines and directives are ignored, as in xcb-util-xrm.
    (is-equal (xrm:put-resource-line db "App.background navy") nil)
    (is-equal (xrm:put-resource-line db "#include \"x\"") nil)
    ;; Whitespace before a name is skipped, but not before ! or #.
    (is-equal (xrm:put-resource-line db " app.z: 1") "1")
    (is-equal (xrm:put-resource-line db " ! comment: 1") nil)))

(deftest test-put-resource ()
  (let ((db (xrm:make-resource-database)))
    (is-equal (xrm:put-resource db "XClock*hands" "red: \\n raw")
              "red: \\n raw")
    ;; The value is stored verbatim, not unescaped.
    (is-equal (lookup db "hands" "Hands" '("xclock" "face") '("XClock" "Face"))
              "red: \\n raw")
    ;; As in xcb-util-xrm, the specifier is not checked separately: in
    ;; "a:b: v" the value starts at the first colon.
    (is-equal (xrm:put-resource db "a:b" "v") "b: v")
    (is-equal (xrm:put-resource db "a b" "v") "v")
    (is-equal (xrm:put-resource db "a/b" "v") nil)))

(deftest test-get-resource-integer ()
  (let ((db (xrm:make-resource-database)))
    (dolist (case '(("n42"   "42")
                    ("nneg"  "-7")
                    ("nplus" "+3")
                    ("nlead" " 42")
                    ("ntrail" "42 ")
                    ("njunk" "4x")))
      (xrm:put-resource db (concatenate 'string "app." (first case))
                            (second case)))
    (flet ((int (name)
             (multiple-value-list
              (xrm:get-resource-integer db name "X" '("app") '("App")))))
      (is-equal (int "n42")    '(42 t "42"))
      (is-equal (int "nneg")   '(-7 t "-7"))
      (is-equal (int "nplus")  '(3 t "+3"))
      (is-equal (int "nlead")  '(nil nil " 42"))
      (is-equal (int "ntrail") '(nil nil "42 "))
      (is-equal (int "njunk")  '(nil nil "4x"))
      (is-equal (int "none")   '(nil nil nil)))))

(deftest test-get-resource-boolean ()
  (let ((db (xrm:make-resource-database)))
    (dolist (case '(("bt" "True") ("bon" "on") ("bno" "NO")
                    ("b0" "0") ("b12" "12") ("bmaybe" "maybe")))
      (xrm:put-resource db (concatenate 'string "app." (first case))
                            (second case)))
    (flet ((bool (name)
             (multiple-value-list
              (xrm:get-resource-boolean db name "X" '("app") '("App")))))
      (is-equal (bool "bt")     '(t t "True"))
      (is-equal (bool "bon")    '(t t "on"))
      (is-equal (bool "bno")    '(nil t "NO"))
      (is-equal (bool "b0")     '(nil t "0"))
      (is-equal (bool "b12")    '(t t "12"))
      (is-equal (bool "bmaybe") '(nil nil "maybe"))
      (is-equal (bool "none")   '(nil nil nil)))))

;;; Encoding

(deftest test-encoding-utf-8 ()
  (let ((utf8 (xrm::string-to-octets (format nil "a~Cb" (code-char 233)))))
    (is-equal (coerce utf8 'list) '(97 195 169 98))
    (is-equal (xrm::octets-to-string utf8) (format nil "a~Cb" (code-char 233)))))

(deftest test-encoding-latin-1-fallback ()
  ;; 233 is not a valid UTF-8 lead byte on its own (its continuation
  ;; bytes are missing), so the bytes decode as Latin-1.
  (is-equal (xrm::octets-to-string (octets 97 233 98))
            (format nil "a~Cb" (code-char 233))))

(deftest test-encoding-rejects-overlong ()
  ;; C0 80 is an overlong encoding of NUL, invalid UTF-8.  A correct
  ;; decoder falls back to Latin-1, producing two characters.
  (is-equal (xrm::octets-to-string (octets 192 128))
            (coerce (list (code-char 192) (code-char 128)) 'string)))

(defun read-octets-file (octets)
  "Read a resource file whose bytes are OCTETS."
  (uiop:with-temporary-file (:pathname p :type "xrdb")
    (with-open-file (out p :direction :output :if-exists :supersede
                           :element-type '(unsigned-byte 8))
      (write-sequence (coerce octets '(vector (unsigned-byte 8))) out))
    (lookup (xrm:read-resources (xrm:make-resource-database) p) "v" "V")))

(deftest test-read-octal-escape-truncates ()
  ;; C stores \777 (511) in a char, keeping the low 8 bits.
  (is-equal (lookup (db-from "app.v: \\777~%") "v" "V") (string (code-char 255)))
  (is-equal (lookup (db-from "app.v: \\101~%") "v" "V") "A"))

(deftest test-read-file-utf-8 ()
  ;; "app.v: é" in UTF-8.
  (is-equal (read-octets-file (append (map 'list #'char-code "app.v: ") '(195 169)))
            (string (code-char 233))))

(deftest test-read-file-latin-1-fallback ()
  ;; "app.v: é" in Latin-1: not valid UTF-8, so it decodes as Latin-1
  ;; instead of signalling.
  (is-equal (read-octets-file (append (map 'list #'char-code "app.v: ") '(233)))
            (string (code-char 233))))

(deftest test-read-file-crlf ()
  ;; As in xcb-util-xrm, lines split on LF only: CR stays in the value.
  (is-equal (read-octets-file
             (map 'list #'char-code (format nil "app.v: x~C~%" #\Return)))
            (format nil "x~C" #\Return)))

(deftest test-question-mark-finalizes-buffer ()
  ;; Unlike xcb-util-xrm's entry.c, we flush the pending name before
  ;; inserting the wildcard, as '.' and '*' do; in the C library
  ;; "abc?def" is the components (WILDCARD "abcdef"), written back as
  ;; "?.abcdef".  We deviate because that contradicts both the
  ;; documented meaning of '?' (one component) and the handling of
  ;; '.' and '*'.  '?' does not reset the binding, so a name after it
  ;; keeps whatever binding preceded it, as in the C.
  (is-equal (xrm:write-resources-to-string (db-from "abc?def: v~%"))
            (format nil "abc.?.def: v~%")))

(deftest test-question-mark-in-value ()
  ;; '?' after the colon is a plain character of the value, in both
  ;; implementations: the parser's chunk is already CS_VALUE.
  (is-equal (lookup (db-from "app.v: a?b~%") "v" "V") "a?b"))

;;; A backend with no server: its RESOURCE_MANAGER is a string or NIL

(defstruct fake-context (resource-manager nil))

(defclass fake-backend (xrm:backend) ())
(defvar *fake-backend* (make-instance 'fake-backend))

(defmethod xrm:context-backend ((c fake-context)) *fake-backend*)
(defmethod xrm:context-default-screen ((c fake-context))
  (declare (ignore c)) :screen)
(defmethod xrm:backend-screen-root ((b fake-backend) c screen)
  (declare (ignore b c screen)) :root)
(defmethod xrm:backend-get-property ((b fake-backend) c window name)
  (declare (ignore b window name))
  (let ((text (fake-context-resource-manager c)))
    (and text (values (xrm::string-to-octets text) 8 :string))))
(defmethod xrm:backend-set-property ((b fake-backend) c window
                                     name type format data)
  (declare (ignore b window name type format))
  (setf (fake-context-resource-manager c) (xrm::octets-to-string data))
  nil)
(defmethod xrm:backend-intern-atom ((b fake-backend) c name)
  (declare (ignore b c)) name)

(deftest test-wm-resources-round-trip ()
  (let ((ctx (make-fake-context))
        (db (xrm:make-resource-database)))
    (xrm:add-resource db '("app" "background") "navy")
    (xrm:set-wm-resources ctx :window db)
    (let ((db2 (xrm:wm-resources ctx :window)))
      (is-equal (lookup db2 "background" "Background") "navy"))))

(deftest test-wm-resources-merges-into-given-db ()
  (let ((ctx (make-fake-context :resource-manager (format nil "app.x: 1~%")))
        (db (xrm:make-resource-database)))
    (xrm:add-resource db '("app" "y") "2")
    (xrm:wm-resources ctx :window :database db)
    (is-equal (lookup db "x" "X") "1")
    (is-equal (lookup db "y" "Y") "2")))

(deftest test-root-resources ()
  (let ((ctx (make-fake-context :resource-manager (format nil "app.background: green~%"))))
    (is-equal (lookup (xrm:root-resources ctx) "background" "Background")
              "green")))

(deftest test-set-root-resources ()
  (let ((ctx (make-fake-context))
        (db (xrm:make-resource-database)))
    (xrm:add-resource db '("app" "background") "blue")
    (xrm:set-root-resources ctx db)
    (is-equal (lookup (xrm:root-resources ctx) "background" "Background")
              "blue")))

;;; default-resources precedence

(defun call-with-home (files fn)
  "Call FN with $HOME a fresh directory holding FILES, an alist of
   (NAME . CONTENTS), and $XENVIRONMENT naming \"xenv\" in it."
  (let* ((dir (uiop:ensure-directory-pathname
               (merge-pathnames (format nil "clxrm-home-~D/" (random 1000000))
                                (uiop:temporary-directory))))
         (old-home (uiop:getenv "HOME"))
         (old-xenv (uiop:getenv "XENVIRONMENT")))
    (ensure-directories-exist dir)
    (unwind-protect
         (progn
           (loop for (name . contents) in files
                 do (with-open-file (out (merge-pathnames name dir)
                                         :direction :output
                                         :if-exists :supersede)
                      (write-string contents out)))
           (setf (uiop:getenv "HOME") (namestring dir)
                 (uiop:getenv "XENVIRONMENT")
                 (namestring (merge-pathnames "xenv" dir)))
           (funcall fn))
      (when old-home (setf (uiop:getenv "HOME") old-home))
      (when old-xenv (setf (uiop:getenv "XENVIRONMENT") old-xenv))
      (uiop:delete-directory-tree dir :validate t :if-does-not-exist :ignore))))

(deftest test-default-resources-precedence ()
  (flet ((bg (ctx)
           (lookup (xrm:default-resources ctx) "background" "Background"
                   '("xclock") '("XClock"))))
    (call-with-home '((".Xresources" . "xclock.background: from-xresources")
                      (".Xdefaults"  . "xclock.background: from-xdefaults"))
      (lambda ()
        ;; RESOURCE_MANAGER wins over any file.
        (is-equal (bg (make-fake-context
                       :resource-manager "xclock.background: from-property"))
                  "from-property")
        ;; With no property: .Xresources, before .Xdefaults.
        (is-equal (bg (make-fake-context)) "from-xresources")))
    (call-with-home '((".Xdefaults" . "xclock.background: from-xdefaults"))
      (lambda ()
        (is-equal (bg (make-fake-context)) "from-xdefaults")))
    ;; $XENVIRONMENT is merged on top of everything.
    (call-with-home '((".Xresources" . "xclock.background: base")
                      ("xenv" . "xclock.background: from-xenvironment"))
      (lambda ()
        (is-equal (bg (make-fake-context)) "from-xenvironment")
        (is-equal (bg (make-fake-context
                       :resource-manager "xclock.background: from-property"))
                  "from-xenvironment")))
    (call-with-home '()
      (lambda ()
        (is-equal (bg (make-fake-context)) nil)))))

;;; Xt layer

(deftest test-xt-names ()
  (is-equal (xrm:resource-name-for :background) "background")
  (is-equal (xrm:resource-name-for :max-width)  "maxWidth")
  (is-equal (xrm:resource-class-for "maxWidth") "MaxWidth"))

(deftest test-xt-convert ()
  (flet ((conv (s type)
           (multiple-value-list (xrm:convert-resource-string s type))))
    (is-equal (conv "300" '(integer 1)) '(300 t))
    (is-equal (conv "abc" '(integer 1)) '(nil nil))
    (is-equal (conv "0.5" 'real) '(0.5d0 t))
    (is-equal (conv "True" 'boolean) '(t t))
    (is-equal (conv "on" 'boolean) '(t t))
    (is-equal (conv "no" 'boolean) '(nil t))
    (is-equal (conv "maybe" 'boolean) '(nil nil))
    (is-equal (conv "tora" '(member :neko :tora)) '(:tora t))
    (is-equal (conv "#102030" '(or null string integer)) '("#102030" t))
    (is-equal (conv "nil" '(or null string)) '(nil t))
    (is-equal (conv " spaced " t) '(" spaced " t))))

(deftest test-xt-lookup-option ()
  (let* ((db (db-from (format nil "XClock.background: navy~%~
                                   xclock.width: 300~%~
                                   xclock.maxWidth: 999~%~
                                   xclock.height: tall~%")))
         (options (list (xrm:make-option :background :default "white"
                                                            :type 'string)
                        (xrm:make-option :width :default 164
                                                      :type '(integer 1))
                        (xrm:make-option :height :default 164
                                                       :type '(integer 1))
                        (xrm:make-option :max-width :default 10
                                                           :type 'integer)
                        (xrm:make-option :padding :default 8
                                                        :type 'integer))))
    (flet ((opt (key &optional plist)
             (multiple-value-list
              (xrm:lookup-option db '("xclock") '("XClock")
                                       options key plist))))
      (is-equal (opt :background) '("navy" :resource))
      (is-equal (opt :width)      '(300 :resource))
      (is-equal (opt :max-width)  '(999 :resource))
      (is-equal (opt :padding)    '(8 :default))
      (is-equal (opt :width '(:width 500)) '(500 :argument))
      (is-equal (opt :background '(:background nil)) '(nil :argument))
      ;; A resource that fails to convert falls back to the default,
      ;; with a warning.
      (is-true (let ((warned nil))
                 (handler-bind ((warning (lambda (w)
                                           (setf warned t)
                                           (muffle-warning w))))
                   (opt :height))
                 warned))
      (is-equal (handler-bind ((warning #'muffle-warning)) (opt :height))
                '(164 :default))
      ;; Undeclared option is an error.
      (is-true (handler-case (progn (opt :nosuch) nil) (error () t)))
      ;; No database: everything is the default.
      (is-equal (multiple-value-list
                 (xrm:lookup-option nil '("xclock") '("XClock")
                                          options :width))
                '(164 :default)))))

;;; Live server, XCB backend: runs only with CLXRM/XCB loaded and
;;; $DISPLAY set; otherwise it passes without doing anything.

(deftest test-xcb-backend-live ()
  ;; Reads RESOURCE_MANAGER, and round-trips UTF-8 text through a
  ;; scratch property on the root window, which it deletes again; the
  ;; user's resources are never written.
  (let ((display (uiop:getenv "DISPLAY")))
    (when (and display (plusp (length display)) (find-package "CLXRM/XCB"))
      (uiop:symbol-call
       '#:xcb '#:call-with-x-connection
       (lambda (conn)
         (let ((backend (xrm:context-backend conn))
               (root (xrm::%screen-root conn nil))
               (text (format nil "clxrm.test: ~C~%" (code-char 233))))
           (is-true (typep (xrm:root-resources conn) 'xrm:resource-database))
           (unwind-protect
                (progn
                  (xrm:backend-set-property
                   backend conn root "_CLXRM_TEST" "STRING" 8
                   (xrm::string-to-octets text))
                  (multiple-value-bind (data format)
                      (xrm:backend-get-property backend conn root "_CLXRM_TEST")
                    (is-equal format 8)
                    (is-equal (xrm::octets-to-string data) text)))
             (uiop:symbol-call '#:xcb '#:delete-property conn root
                               (xrm:backend-intern-atom backend conn "_CLXRM_TEST"))
             (uiop:symbol-call '#:xcb '#:get-input-focus-reply conn
                               (uiop:symbol-call '#:xcb '#:get-input-focus conn)))
           (is-false
            (xrm:backend-get-property backend conn root "_CLXRM_TEST"))))))))

;;; Runner

(defun run-tests ()
  "Run the clXrm tests.  Returns T if all passed."
  (do-tests))
