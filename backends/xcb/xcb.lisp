;;;; xcb.lisp — clXrm backend for CLXCB
;;;; Copyright (C) 2026 Arthur Miller
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The context is an XCB:XCB-CONNECTION, a screen an XCB:SCREEN, a
;;;; window a window ID (an integer).
;;;;
;;;; Only symbols CLXCB exports are used: the generated xproto requests
;;;; (GET-PROPERTY, CHANGE-PROPERTY, GET-INPUT-FOCUS) and reply
;;;; accessors, and the hand-written INTERN-ATOM-ID.  CLXCB's API
;;;; is not yet frozen, so a new CLXCB version may need this file
;;;; updated.

(defpackage #:clxrm/xcb
  (:use #:cl)
  (:export #:xcb-backend)
  (:documentation "clXrm backend for CLXCB: lets the clXrm server
   functions take an XCB:XCB-CONNECTION as their context."))

(in-package #:clxrm/xcb)

(defclass xcb-backend (xrm:backend) ())

(defparameter *backend* (make-instance 'xcb-backend))

(defconstant +any-property-type+ 0
  "The GetProperty wildcard type, AnyPropertyType.")

(defconstant +prop-mode-replace+ 0)

(defconstant +initial-long-length+ (* 16 1024)
  "How much of a property to ask for first, in 32-bit units, as
   xcb-util-xrm's xcb_xrm_database_from_resource_manager does.")

(defmethod xrm:context-backend ((context xcb:xcb-connection))
  *backend*)

(defmethod xrm:context-default-screen ((context xcb:xcb-connection))
  (first (xcb:conn-roots context)))

(defmethod xrm:backend-screen-root ((backend xcb-backend) context (screen xcb:screen))
  (declare (ignore context))
  (xcb:root screen))

(defmethod xrm:backend-get-property ((backend xcb-backend) conn window name)
  ;; ONLY-IF-EXISTS: an atom nobody interned cannot name a property.
  (let ((atom (xcb:intern-atom-id conn name t)))
    (when (zerop atom)
      (return-from xrm:backend-get-property (values nil nil nil)))
    ;; As xcb-util-xrm's xcb_util_get_property: if the property is
    ;; longer than what was asked for, ask again for all of it.
    (loop with long-length = +initial-long-length+
          for reply = (xcb:get-property-reply
                       conn (xcb:get-property conn 0 window atom +any-property-type+
                                              0 long-length))
          do (cond ((zerop (xcb:value-len reply))
                    (return (values nil nil nil)))
                   ((plusp (xcb:bytes-after reply))
                    (incf long-length (ceiling (xcb:bytes-after reply) 4)))
                   (t
                    (return (values (coerce (xcb:value reply)
                                            '(simple-array (unsigned-byte 8) (*)))
                                    (xcb:format reply)
                                    (xcb:type reply))))))))

(defmethod xrm:backend-set-property ((backend xcb-backend) conn window
                                        name type format data)
  (check-type format (member 8 16 32))
  ;; CHANGE-PROPERTY takes the octets as they are and the length in
  ;; FORMAT-bit items; SEND-CHANGE-PROPERTY would pack each octet as a
  ;; whole item.
  (xcb:change-property conn +prop-mode-replace+ window
                       (xcb:intern-atom-id conn name)
                       (if (stringp type)
                           (xcb:intern-atom-id conn type)
                           type)
                       format
                       (floor (length data) (floor format 8))
                       data)
  ;; A round trip, as libxcb-util's xcb_aux_sync does it: the server
  ;; has processed the ChangeProperty by the time this reply arrives.
  (xcb:get-input-focus-reply conn (xcb:get-input-focus conn))
  nil)

(defmethod xrm:backend-intern-atom ((backend xcb-backend) conn name)
  (xcb:intern-atom-id conn name))
