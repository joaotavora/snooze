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
  "If non-NIL, catch HTTP conditions and explain them to the client.

If T (the default,) this means that any conditions which are *not a
fault of the server* (thus excluding HTTP 500-class conditions) will
generate a response to the user-agent. To generate this response,
Snooze will first try EXPLAIN-CONDITION to \"politely\" explain the
condition in a format accepted by the user-agent, as indicated in the
\"Accept:\" header. the condition to the client. If that fails, the
condition is presented very succintly to the client.

If the value is the keyword :VERBOSE, don't call EXPLAIN-CONDITION and
present a plain-text explanation with a full Lisp backtrace indicating
where the condition originated. This options is useful during
development.

If set to NIL errors will bubble up out of snooze and possible land
you in the debugger. This option is useful during development if you
prefer an interactive debugger.")

(defparameter *catch-errors* t
  "If non-NIL, catch any error and explain them to the client.

If T (the default,) this means that *any* erroneous conditions that
are a *fault of the server and not of the user-agent* (and this
includes HTTP 500-class conditions) will generate a response to the
user-agent. To generate this response, Snooze will first try
EXPLAIN-CONDITION to \"politely\" explain the condition in a format
accepted by the user-agent, as indicated in the \"Accept:\"
header. the condition to the client. If that fails, the error is
presented very succintly to the client.

If the value is the keyword :VERBOSE, don't call EXPLAIN-CONDITION and
offer a plain text explanation with a full Lisp backtrace indicating
where the condition originated. This options is useful during
development.

If set to NIL errors will bubble up out of snooze and possible land
you in the debugger. This option is useful during development if you
prefer an interactive debugger")

(defvar *resource-filter* (constantly t)
  "Tell if a resource should be considered when handling requests.

Value is a function designator called with a resource. This function
should return a boolean.

The default value is (CONSTANTLY T) matching every resource defined so
far by DEFRESOURCE and DEFROUTE.

Can be let-bound to restrict searches by a particular server to a
specific set of resources.")

(defparameter *resource-name-function* 'default-resource-name
  "How to search for resource names in URI paths.

Value is a function designator called on every request with the
request's URI path. The function might be called with the empty
string.

This function should return two values: a resource designator (a
string, symbol or a resource) and relative URI string stripped of the
resource-designating part.  If the first value returned is nil,
*HOME-RESOURCE* is used to lookup a suitable resource.

The function should *not* attempt any URI-decoding of the component
string. That is done automatically elsewhere.

The default value is DEFAULT-RESOURCE-NAME, which return the first
path component as the first value and the remaining URI as the second
value..

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
string representing a (quite possibly) encoded URI.

This function should return two values: a list of content-type
designators and the rewritten URI path stripped of its
content-designating componenets. The function can also return NIL.

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

(defgeneric uri-to-arguments (resource relative-uri)
  (:documentation
   "Extract arguments for resource from RELATIVE-URI.

RELATIVE-URI is a string, where everything but the part designating
RESOURCE has been kept untouched (and potentially URI-encoded)

Should return two values: a list of \"plain\" arguments and an
alist (*not* a plist) used as keyword arguments.

It's reasonable for user-written specializaions of this method to
error out with 400 (malformed) or 404 not found status codes. At any
rate.

This method is the inverse of ARGUMENTS-TO-URI

The default method turns the path section of RELATIVE-URI into
\"plain\" args and the query and fragment sections of URI into keyword
arguments. READ-FROM-STRING is used to convert every individual
argument's value. If an argument is unconvertible, an HTTP 400
condition of type UNCONVERTIBLE-ARGUMENT is signalled."))

(defgeneric arguments-to-uri (resource plain-args keyword-args)
  (:documentation
   "Generate an URI path string to fir RESOURCE.
PLAIN-ARGS and KEYWORD-ARGS are like the return values of
URI-TO-ARGUMENTS.

Should return a propertly escaped URI path that will display in the
address bar and/or be sent on future requests.

This method is the inverse of URI-TO-ARGUMENTS.

The default method tries to WRITE-TO-STRING (with *PRINT-CASE* set
to :DOWNCASE) every object, except for keywords, which are written
without the leading \":\" character. Afterwards the whole URI is
escaped for invalid sequences."))
  

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

(defvar *backend*)
(setf (documentation '*backend* 'variable)
      "Bound to a keyword identifying the handling server backend.")

(defvar *clack-request-env*)
(setf (documentation '*clack-request-env* 'variable)
      "Bound in function made by MAKE-CLACK-APP to Clack environment.")

(defun make-clack-app (&optional bindings)
  "Make a basic Clack app that calls HANDLE-REQUEST.

Dynamically binds *CLACK-REQUEST-ENV* around every call to
HANDLE-REQUEST so you can access the backend-specific from routes
and/or EXPLAIN-CONDITION. Also binds *BACKEND* to :CLACK.

BINDINGS is an alist of (SYMBOL . VALUE) which is are also
dynamically-bound around HANDLE-REQUEST. You can use it to pass values
of special variables that affect Snooze, like *HOME-RESOURCE*,
*RESOURCES-FUNCTION*, *RESOURCE-NAME-FUNCTION*, or
*URI-CONTENT-TYPES-FUNCTION*."
  (lambda (env) (let ((*clack-request-env* env)
                      (*backend* :clack))
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
                        (,payload)))))))

(defun payload-as-string () (payload-as-string-1 *backend*))




