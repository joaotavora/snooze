(defpackage #:snooze-common
  (:use #:cl)
  (:export
   #:ensure-atom
   #:make-genpath-form
   #:content-type-spec-or-lose
   #:verb-spec-or-lose
   #:parse-defroute-args
   #:supertype-metaclass
   #:scan-to-strings*
   #:find-content-class
   #:probe-class-sym
   #:*mime-type-hash*
   #:resource-generic-function
   #:find-verb-or-lose
   #:parse-content-type-header
   #:arglist-compatible-p
   #:prefilter-accepts-header
   #:parse-resource
   #:content-classes-in-accept-string
   #:gf-primary-method-specializer
   #:debug
   #:debug-condition
   #:destructive-p
   #:content-class-name
   ;;
   #:find-resource
   #:delete-resource
   ;;
   #:*resource-name-function*
   #:*resources-function*
   #:*home-resource*
   #:*uri-content-types-function*
   #:*all-resources*
   #:resource-name
   #:parse-keywords-in-uri))

(defpackage #:snooze
  (:use #:cl #:snooze-common)
  (:import-from #:snooze-common
                #:*resource-name-function*
                #:*resources-function*
                #:*home-resource*
                #:*uri-content-types-function*
                ;;
                #:find-resource
                #:delete-resource
                )
  (:nicknames #:rip)
  (:export
   ;; server configuration
   ;;
   #:*resource-name-function*
   #:*resources-function*
   #:*home-resource*
   #:*uri-content-types-function*
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
   #:no-such-resource
   #:invalid-resource-arguments
   #:no-such-route
   #:no-matching-content-types
   #:explain-condition
   ;; request handling helpers
   ;; 
   #:request-body
   #:handle-request
   #:*catch-errors*
   #:*catch-http-conditions*
   ;; backend stuff
   ;; 
   #:*clack-request-env*
   #:make-clack-app
   #:cannot-convert-argument
   #:unconvertible-value))

(defpackage :snooze-types (:use) (:export #:content))
