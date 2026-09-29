;;;; clxrm.asd — the X resource manager, for CLX and CLXCB
;;;; Copyright (C) 2026 Arthur Miller
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; clxrm             the core: databases, matching, files, Xt-style
;;;;                    options, and RESOURCE_MANAGER through a backend
;;;;                    protocol.  Does not depend on X.
;;;; clxrm/clx         backend for CLX (context: XLIB:DISPLAY)
;;;; clxrm/xcb         backend for CLXCB (context: XCB:XCB-CONNECTION)
;;;; clxrm/all         everything
;;;; clxrm/tests       the test suite (needs no X server)

(require :asdf)
(cl:in-package #:asdf-user)

(defsystem "clxrm"
  :description "The X resource manager (Xrm) for Common Lisp."
  :long-description
  "A port of xcb-util-xrm's parser, database and matching algorithm,
with CLX's resource API, so that CLX and CLXCB programs can share it
through small backends.  It implements xrdb resource databases, the Xrm
matching algorithm, and the RESOURCE_MANAGER property."
  :author "Arthur Miller <arthur.miller@live.com>"
  :license "MIT"
  :version "0.5.0"
  :depends-on ("uiop")
  :in-order-to ((test-op (test-op "clxrm/tests")))
  :pathname "src/"
  :serial t
  :components ((:file "package")
               (:file "encoding")
               (:file "database")
               (:file "resources")
               (:file "options")
               (:file "backend")
               (:file "server")))

(defsystem "clxrm/clx"
  :description "clXrm backend for CLX."
  :author "Arthur Miller <arthur.miller@live.com>"
  :license "MIT"
  :depends-on ("clxrm" "clx")
  :pathname "backends/clx/"
  :components ((:file "clx")))

(defsystem "clxrm/xcb"
  :description "clXrm backend for CLXCB."
  :author "Arthur Miller <arthur.miller@live.com>"
  :license "MIT"
  :depends-on ("clxrm" "clxcb")
  :pathname "backends/xcb/"
  :components ((:file "xcb")))

;;;; clxrm/tests — RT-based tests; SBCL uses its bundled sb-rt,
;;;; other implementations must supply the RT system themselves.

;; SBCL bundles RT as the sb-rt contrib; pull it in now so that the
;; package exists by the time the test file is read.  Everywhere else
;; the :depends-on entry below makes ASDF find an "rt" system.
#+sbcl (eval-when (:load-toplevel :execute) (require :sb-rt))

;; we can load both for testing purpose
(defsystem "clxrm/all"
  :description "clXrm with both backends and the CLX compatibility API."
  :license "MIT"
  :depends-on ("clxrm" "clxrm/clx" "clxrm/xcb"))

(defsystem "clxrm/tests"
  :description "RT-based tests for clXrm; no X server needed."
  :license "MIT"
  :depends-on ("clxrm"
               #-sbcl "rt")
  :pathname "tests/"
  :components ((:file "tests"))
  :perform (test-op (o c)
             (unless (uiop:symbol-call '#:clxrm/tests '#:run-tests)
               (error "clXrm RT tests failed."))))
