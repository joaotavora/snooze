;;; User facing API.
;;;
(in-package #:snooze)

(defmacro defresource (name lambda-list &rest options)
  "Define NAME as a generic REST resource.
LAMBDA-LIST is a lambda list with at least two mandatory arguments,
which stand for the HTTP method and content-type respectively. Further
arguments (mandatory, optional or keyword) determine how the resource
can be accessed. OPTIONS is a list of options like in CL:DEFGENERIC,
with the following additional options: :GENPATH and :ROUTE"
  (defresource-1 name lambda-list options))

(defmacro defroute (name &body args)
  "Define NAME as a specific route on a generic REST resource.
ARGS are just as in CL:DEFMETHOD with the exception that its
specialized-lambda-list portion accepts some simplifications."
  (defroute-1 name args))

(defparameter *catch-http-conditions* t
  "If non-nil, catch HTTP conditions and explain them to the client.
If T, an attempt is made to \"politely\" explain the condition to the
client in a format accepted by the user agent as indicated in the
\"Accept:\" header. If the value is the keyword :BACKTRACE, offer a
plain text explanation with a full Lisp backtrace indicating where the
condition originated.")

(defparameter *catch-errors* t
  "If non-nil, catch any error and explain them to the client.
If T, an attempt is made to \"politely\" explain the condition to the
client in a format accepted by the user agent as indicated in the
\"Accept:\" header. If the value is the keyword :BACKTRACE, offer a
plain text explanation with a full Lisp backtrace indicating where the
condition originated.")

(defvar *all-resources* nil
  "A list of all resource defined.
Can be let bound to restrict searchs by HANDLE-REQUEST to a
particular set of resources.")

(defparameter *resource-name-regexp* "/([^/.]+)"
  "How to search for resource names in URI paths.
Value is a string containing a regular expression. The first capturing
group of this regular expression should capture the resource's
name.")

(defparameter *home-resource* :home
  "Default resource to serve when request's URI path is empty.
Value is a resource designator: a string, a keyword, a symbol or
generic function designating a resource in *ALL-RESOURCES*, probably
created with DEFRESOURCE or DEFROUTE.

A string or keyword value causes *ALL-RESOURCES* to be scanned by the
resource's name.")

(defun http-condition (status-code
                       &optional (format-control nil format-control-supplied-p)
                                 (format-args nil format-args-supplied-p))
  "Signal an HTTP condition with STATUS-CODE with with CL:ERROR."
  (apply #'error 'http-condition :status-code status-code
         `(,@(if format-args-supplied-p
                 `(:format-arguments ,format-args))
           ,@(if format-control-supplied-p
                 `(:format-control ,format-control)))))

(defgeneric explain-condition (condition resource content-type )
  (:documentation "Explain CONDITION for RESOURCE in CONTENT-TYPE."))

(defgeneric convert-arguments (resource plain-arguments keyword-arguments)
  (:documentation
   "In the context of SERVER, make ACTUAL-ARGUMENTS fit RESOURCE.
Should return a list of the same length as ACTUAL-ARGUMENTS, which is
a list of strings, but where some strings have been converted to other
types.  The default method tries to convert every argument to a
number, otherwise leaves it uncoverted in string form."))

(defparameter *allow-extension-as-accept* t
  "If non-NIL, filename extensions in URI path add to \"Accept:\"")

(defun handle-request (uri &key
                             (method :get)
                             (accept "*/*")
                             (content-type "application/x-www-form-urlencoded"))
  "Handles a HTTP request for URI.
METHOD a keyword, string or symbol designating the HTTP method (or
\"verb\"). ACCEPT is a string in the format of the \"Accept:\"
header. IN-CONTENT-TYPE is a string in the format of the
\"Content-Type\" header in the request.

Returns three values CODE, PAYLOAD and OUT-CONTENT-TYPE, which should
be used by the application to craft a response to the request."
  (handle-request-1 uri method accept content-type))

(defvar *clack-request-env*
  "Bound in function made by MAKE-CLACK-APP to Clack environment.")

(defun make-clack-app ()
  "Make a basic CLACK app that calls HANDLE-REQUEST.
Binds *CLACK-REQUEST-ENV* so you can access the original request from
routes and/or EXPLAIN-CONDITIONS"
  (lambda (env) (let ((*clack-request-env* env))
                  (multiple-value-bind (status-code payload payload-ct)
                      (handle-request (getf env :request-uri)
                             :method (getf env :request-method)
                             :accept (gethash "accept" (getf env :headers))
                             :content-type (getf env :content-type))
                    `(,status-code
                      (:content-type ,payload-ct)
                      (,payload))))))




