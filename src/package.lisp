;;;; package.lisp — the core package, XRM (nickname CLXRM)
;;;; Copyright (C) 2026 Arthur Miller
;;;; SPDX-License-Identifier: MIT

(defpackage #:xrm
  (:nicknames #:clxrm)
  (:use #:cl)
  (:export
   ;; Database (database.lisp) -- names and lambda lists as in CLX
   #:resource-key
   #:resource-database
   #:make-resource-database
   #:resource-database-timestamp
   #:stringable
   #:*uppercase-resource-symbols*
   #:add-resource
   #:delete-resource
   #:get-resource
   #:get-resource-integer
   #:get-resource-boolean
   #:get-search-table
   #:get-search-resource
   #:map-resource
   #:merge-resources

   ;; Resource files (resources.lisp)
   #:read-resources
   #:read-resources-from-string
   #:write-resources
   #:write-resources-to-string
   #:parse-resource
   #:put-resource
   #:put-resource-line

   ;; Xt conventions (options.lisp): option from argument, else
   ;; resource, else default
   #:resource-name-for
   #:resource-class-for
   #:convert-resource-string
   #:option
   #:make-option
   #:option-key
   #:option-resource-name
   #:option-resource-class
   #:option-default
   #:option-type
   #:lookup-resource-value
   #:lookup-option
   #:define-option-set

   ;; Server interaction (server.lisp)
   #:root-resources
   #:set-root-resources
   #:wm-resources
   #:set-wm-resources
   #:default-resources
   #:default-resources-pathname
   #:xresources-pathname
   #:resources-pathname

   ;; Backend protocol (backend.lisp), for backend implementors
   #:backend
   #:context-backend
   #:context-default-screen
   #:backend-screen-root
   #:backend-get-property
   #:backend-set-property
   #:backend-intern-atom)

  (:documentation
   "The X resource manager (Xrm): resource databases, the
   Xrm matching algorithm, resource files, and the RESOURCE_MANAGER
   property.  Parsing, matching and writing are a port of xcb-util-xrm
   and give its results, including the wildcard ?; the function names
   and lambda lists are CLX's.  The core knows nothing about CLX or XCB; the server
   functions go through the generic protocol in backend.lisp, which the
   CLXRM/CLX and CLXRM/XCB systems implement."))
