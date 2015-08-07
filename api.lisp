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

(defvar *resources-function* 'all-defined-resources
  "Compute list of all resources to consider when handling requests.

Value is a function designator called with no arguments. This function
should return a list of resources.

The default value is ALL-DEFINED-RESOURCES, which returns a list of
every resource defined so far by DEFRESOURCE and DEFROUTE.

Can be let-bound to restrict searches by a particular server to a
specific set of resources.")

(defparameter *resource-name-function* 'default-resource-name
  "How to search for resource names in URI paths.
Value is a function designator called on every request with multiple arguments,
one for each component of the request's URI path (not the query and
fragment compoenents).  The function might be called with no arguments
at all if the path is the root path.

This function should return two values: a resource designator (a
string, symbol or a resource) and a partial list of arguments to pass
to the resource.  If the first value returned is nil, *HOME-RESOURCE*
is used to lookup a suitable resource.

The default value is DEFAULT-RESOURCE-NAME, which return the first
component as the first value and the remaining components are the
second value. This means that an the URI path \"foo/bar/baz\"
designates the \"foo\" resource called with arguments \"bar\" and
\"baz\".

Can be let-bound to modify the URI scheme used by a particular
server.")

(defparameter *home-resource* :home
  "Default resource to serve when request's URI path is empty.
Value is a resource designator: a string, a keyword, a symbol or
generic function designating a resource as given by
*ALL-RESOURCES*.

A string or keyword value causes *ALL-RESOURCES* to be scanned by the
resource's name.")

(defparameter *uri-content-types-function* 'search-for-extension-content-type
  "Compute list of content types encoded in URI paths.
If the value is non-NIL, it must be a function of a single argument, a
string. This function should return two values: a list of content-type
designators and the rewritten URI path stripped of its
content-designating componenets.

The default value is SEARCH-FOR-EXTENSION-CONTENT-TYPE, which looks
for the first filename extension in the URI path, returns a list of
the suitable content-type as the first value and, as the second value,
the URI path stripped of the extension.

Can be let-bound to modify the URI scheme used by a particular
server.")

(defun http-condition (status-code
                       &optional
                         format-control
                       &rest format-args)
  "Signal an HTTP condition with STATUS-CODE with with CL:ERROR."
  (error 'http-condition :status-code status-code
                         :format-control format-control
                         :format-arguments format-args))

(defgeneric explain-condition (condition resource content-type )
  (:documentation "Explain CONDITION for RESOURCE in CONTENT-TYPE."))

(defgeneric convert-arguments (resource plain-args keyword-args)
  (:documentation
   "Massage PLAIN-ARGS and KEYWORD-ARGS fit RESOURCE.
PLAIN-ARGS and KEYWORD-ARGS are extracted from the request URI path.
Every element of PLAIN-ARGS is a strings, as are the even numbered
elements of KEYWORD-ARGS. The odd-numbered values of of KEYWORD-ARGS
are the symbols of the keyword arguments defined in RESOURCE.

Should return two values: a list of values for plain, non-keyword
parameters and a plist of keyword arguments.

The default method tries to convert every argument to a number,
otherwise leaves it uncoverted in string form."))

(defun handle-request (uri &key
                             (method :get)
                             (accept "*/*")
                             (content-type "application/x-www-form-urlencoded"))
  "Dispatches an HTTP request for URI to the appropriate resource.

METHOD a keyword, string or symbol designating the HTTP method (or
\"verb\"). ACCEPT is a string in the format of the \"Accept:\"
header. IN-CONTENT-TYPE is a string in the format of the
\"Content-Type\" header in the request.

Returns three values CODE, PAYLOAD and OUT-CONTENT-TYPE, which should
be used by the application to craft a response to the request."
  (handle-request-1 uri method accept content-type))

(defvar *clack-request-env*
  "Bound in function made by MAKE-CLACK-APP to Clack environment.")

(defun make-clack-app (&optional bindings)
  "Make a basic Clack app that calls HANDLE-REQUEST.

Dynamically binds *CLACK-REQUEST-ENV* around every call to
HANDLE-REQUEST so you can access the backend-specific from routes
and/or EXPLAIN-CONDITION.

BINDINGS is an alist of (SYMBOL . VALUE) which is are also
dynamically-bound around HANDLE-REQUEST. You can use it to pass values
of special variables that affect Snooze, like *HOME-RESOURCE*,
*RESOURCES-FUNCTION*, *RESOURCE-NAME-FUNCTION*, or
*URI-CONTENT-TYPES-FUNCTION*."
  (lambda (env) (let ((*clack-request-env* env))
                  (progv
                      (mapcar #'car bindings)
                      (mapcar #'cdr bindings)
                    (multiple-value-bind (status-code payload payload-ct)
                        (handle-request (getf env :request-uri)
                                        :method (getf env :request-method)
                                        :accept (gethash "accept" (getf env :headers))
                                        :content-type (getf env :content-type))
                      `(,status-code
                        (:content-type ,payload-ct)
                        (,payload))))))))




