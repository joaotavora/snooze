(defpackage #:snooze
  (:use :cl)
  (:nicknames :rip)
  (:export
   #:defroute
   #:rest-acceptor
   #:define-content
   #:fragment
   #:|404|
   #:explain-condition
   #:defresource
   #:content-body
   #:route-packages
   #:*always-explain-conditions*
   #:start
   #:stop
   #:snooze-server
   #:http-condition
   #:parse-uri
   #:code
   #:arglist-compatible-p
   #:fall-through-p
   #:home-resource
   #:parse-accept-header
   #:convert-arguments
   #:parse-content-type-header
   #:probe-class-sym
   #:no-such-route
   #:no-matching-content-types))

(defpackage #:snooze-backend
  (:use :cl)
  (:nicknames :snooze-backend)
  (:export
   #:backend
   #:start
   #:stop
   #:started-p
   #:backend-class))
