(in-package :cl)

(defpackage #:snooze-common
  (:use #:cl)
  (:export
   #:ensure-atom
   #:make-genurl-form
   #:content-type-spec-or-lose
   #:verb-spec-or-lose
   #:parse-defroute-args
   #:supertype-metaclass
   #:scan-to-strings*
   #:find-content-class
   #:check-arguments
   #:probe-class-sym
   #:*mime-type-hash*))

(defpackage #:snooze
  (:use #:cl #:snooze-common)
  (:nicknames #:rip)
  (:export
   ;; server configuration
   ;; 
   #:snooze-server
   #:route-packages
   #:resource-name-regexp
   #:fall-through-p
   #:home-resource
   ;; server control
   ;; 
   #:start
   #:stop
   #:started-p
   ;; route definition
   ;; 
   #:defresource
   #:defroute
   #:fragment
   #:convert-arguments
   ;; error handling
   ;; 
   #:http-condition
   #:status-code
   #:http-error
   #:|404|
   #:no-such-resource
   #:invalid-resource-arguments
   #:no-such-route
   #:no-matching-content-types
   #:explain-condition
   #:*always-explain-conditions*
   ;; request handling helpers
   ;; 
   #:request-body))

(defpackage #:snooze-utils
  (:use #:cl #:snooze-common)
  (:export
   #:parse-uri
   #:arglist-compatible-p
   #:parse-content-type-header
   #:find-verb-or-lose
   #:prefilter-accepts-header))

(defpackage #:snooze-backend
  (:use :cl)
  (:export
   #:server
   #:backend
   #:start
   #:stop
   #:started-p
   #:backend-class
   #:request-body
   #:*current-server*))
