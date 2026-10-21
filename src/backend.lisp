;;;; backend.lisp — the protocol between the Xrm core and an X library
;;;; Copyright (C) 2026 Arthur Miller
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The core reaches the server only through these generic functions.  A
;;;; backend implements them for its connection class; CLXRM/CLX does it for
;;;; XLIB:DISPLAY and CLXRM/XCB for XCB:CONNECTION.  Nothing here knows
;;;; about Xrm, and nothing in the rest of the core knows which backend it is
;;;; talking to.
;;;;
;;;; This separation is done so one can use clxrm with both CLX programs and
;;;; CLXCB programs.  Not that I believe lots of new programs using CLX library
;;;; are written today, but we can, so why not.

(in-package #:xrm)

(defclass backend () ()
  (:documentation "Base class for X server backends.  A backend reads
   and writes properties on a connection, interns atoms, and finds a
   screen's root window.  It knows nothing about Xrm."))

(defgeneric context-backend (context)
  (:documentation "Return the BACKEND that talks to CONTEXT: an
   XLIB:DISPLAY for the CLX backend, an XCB:CONNECTION for the XCB
   backend, or any object whose class has these methods.")
  (:method (context)
    (error "No clXrm backend for ~S.  Load CLXRM/CLX or CLXRM/XCB, ~
            or define CONTEXT-BACKEND for its class." context)))

(defgeneric context-default-screen (context)
  (:documentation "Return CONTEXT's default screen, in the backend's own
   representation."))

(defgeneric backend-screen-root (backend context screen)
  (:documentation "Return the root window of SCREEN on CONTEXT, in the
   form BACKEND-GET-PROPERTY accepts as a window."))

(defgeneric backend-get-property (backend context window name)
  (:documentation "Read property NAME (a string) of WINDOW on CONTEXT.
   Returns (VALUES DATA FORMAT TYPE): DATA is the property's value as
   raw octets (for FORMAT 16 and 32, in the server's byte order), or NIL
   when the property does not exist or is empty; FORMAT is 8, 16 or 32;
   TYPE is the property's type, opaque to the core.  The core itself
   only reads RESOURCE_MANAGER, a format 8 STRING, and the CLX backend
   supports only format 8: it returns NIL for other formats."))

(defgeneric backend-set-property (backend context window name type format data)
  (:documentation "Replace property NAME (a string) of WINDOW on CONTEXT
   with DATA, a vector of raw octets (for FORMAT 16 and 32, in the
   byte order the backend's connection uses; the CLX backend accepts
   only FORMAT 8), of type TYPE (a string naming an atom,
   such as \"STRING\") and format FORMAT.  Must not return until the
   server has processed the request (a round trip), so that other
   clients -- xrdb, another program -- see the new value as soon as the
   caller's SET-ROOT-RESOURCES returns: X orders requests only within
   one connection."))

(defgeneric backend-intern-atom (backend context name)
  (:documentation "Return the atom for NAME, a string, on CONTEXT."))
