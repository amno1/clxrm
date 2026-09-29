;;;; encoding.lisp — text <-> octets, for X properties and resource files
;;;; Copyright (C) 2026 Arthur Miller
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; X's STRING properties are Latin-1 (ICCCM), but xrdb copies
;;;; the bytes of the resource file verbatim. On current systems those
;;;; are UTF-8.  So bytes are decoded as UTF-8 if they are valid UTF-8
;;;; and as Latin-1 otherwise (pure ASCII is the same either way), and
;;;; written as UTF-8, which is what xrdb itself would have stored.
;;;; Resource files are read and written the same way.
;;;;
;;;; On SBCL, OCTETS-TO-STRING and STRING-TO-OCTETS are SB-EXT's, which
;;;; are implemented in C; the same UTF-8/Latin-1 policy is expressed
;;;; by catching SB-EXT's decoding error and retrying as Latin-1.
;;;; Anywhere else we use portable versions below.

(in-package #:xrm)

(deftype octet-vector () '(simple-array (unsigned-byte 8) (*)))

#+sbcl
(progn
  (declaim (inline octets-to-string string-to-octets))

  (defun octets-to-string (octets)
    "Decode OCTETS, a vector of octets, as UTF-8 if they are valid UTF-8,
     and as Latin-1 otherwise."
    (declare (type octet-vector octets))
    (handler-case
        (sb-ext:octets-to-string octets :external-format :utf-8)
      ;; SB-INT:CHARACTER-DECODING-ERROR is not exported from SB-EXT, but
      ;; it is the condition SB-EXT:OCTETS-TO-STRING signals on invalid
      ;; input, and there is no exported alternative that lets us avoid
      ;; decoding twice.  This is the one place clXrm depends on an
      ;; internal SBCL symbol; if a future SBCL renames it, this is the
      ;; only line to change.  Other errors are bugs and should propagate.
      (sb-int:character-decoding-error ()
        (sb-ext:octets-to-string octets :external-format :latin-1))))

  (defun string-to-octets (string)
    "Encode STRING as UTF-8, returning an OCTET-VECTOR."
    (declare (type string string))
    (sb-ext:string-to-octets string :external-format :utf-8)))

#-sbcl
(progn
  (defun %decode-utf-8 (octets)
    "Decode OCTETS as UTF-8.  Returns the string, or NIL if OCTETS is not
     valid UTF-8: a bad lead or continuation byte, a truncated sequence,
     an overlong encoding, a surrogate, a value above #x10FFFF, or a
     character the Lisp cannot represent."
    (let ((out (make-string-output-stream))
          (n (length octets))
          (i 0))
      (loop while (< i n)
            do (let* ((b (aref octets i))
                      (len (cond
                             ((< b #x80) 1)
                             ((<= #xC2 b #xDF) 2)
                             ((<= #xE0 b #xEF) 3)
                             ((<= #xF0 b #xF4) 4)
                             (t (return-from %decode-utf-8 nil))))
                      (code (if (= len 1)
                                b
                                (ldb (byte (- 7 len) 0) b))))
                 (when (> (+ i len) n)
                   (return-from %decode-utf-8 nil))
                 (loop for k from 1 below len
                       for c = (aref octets (+ i k))
                       do (unless (= (logand c #xC0) #x80)
                            (return-from %decode-utf-8 nil))
                          (setf code (logior (ash code 6) (logand c #x3F))))
                 (unless (and (>= code
                                  (ecase len
                                    (1 0)
                                    (2 #x80)
                                    (3 #x800)
                                    (4 #x10000)))
                              (not (<= #xD800 code #xDFFF))
                              (<= code #x10FFFF)
                              (< code char-code-limit))
                   (return-from %decode-utf-8 nil))
                 (write-char (code-char code) out)
                 (incf i len)))
      (get-output-stream-string out)))

  (defun %decode-latin-1 (octets)
    (map 'string #'code-char octets))

  (declaim (inline octets-to-string))
  (defun octets-to-string (octets)
    "Decode OCTETS, a vector of octets, as UTF-8 if they are valid UTF-8,
     and as Latin-1 otherwise."
    (declare (type octet-vector octets))
    (or (%decode-utf-8 octets) (%decode-latin-1 octets)))

  (declaim (inline string-to-octets))
  (defun string-to-octets (string)
    "Encode STRING as UTF-8, returning an OCTET-VECTOR."
    (declare (type string string))
    (let ((out (make-array (length string) :element-type '(unsigned-byte 8)
                                           :adjustable t :fill-pointer 0)))
      (flet ((put (b) (vector-push-extend b out)))
        (loop for ch across string
              for code = (char-code ch)
              do (cond ((< code #x80) (put code))
                       ((< code #x800)
                        (put (logior #xC0 (ash code -6)))
                        (put (logior #x80 (ldb (byte 6 0) code))))
                       ((< code #x10000)
                        (put (logior #xE0 (ash code -12)))
                        (put (logior #x80 (ldb (byte 6 6) code)))
                        (put (logior #x80 (ldb (byte 6 0) code))))
                       (t
                        (put (logior #xF0 (ash code -18)))
                        (put (logior #x80 (ldb (byte 6 12) code)))
                        (put (logior #x80 (ldb (byte 6 6) code)))
                        (put (logior #x80 (ldb (byte 6 0) code)))))))
      (coerce out 'octet-vector))))
