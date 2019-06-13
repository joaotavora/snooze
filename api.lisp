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

(defmacro defgenpath (resource function)
  "Define FUNCTION as a path-generating function for RESOURCE.
RESOURCE is a symbol or a string designating a resource"
  (defgenpath-1 function resource))

(defparameter *catch-http-conditions* t
  "If non-NIL, catch HTTP conditions and explain them to the client.

If T (the default), any conditions which are *not a fault of the
server* (thus excluding HTTP 500-class conditions), will result in a
response to the user-agent.

To compose this response, Snooze will first try EXPLAIN-CONDITION to
\"politely\" explain the condition in a format accepted by the
user-agent, as indicated in the \"Accept:\" header. If that fails, the
condition is presented very succintly to the client.

If the value is the keyword :VERBOSE, don't call EXPLAIN-CONDITION and
present a plain-text explanation with a full Lisp backtrace indicating
where the condition originated. This option is useful during
development.

If set to NIL, HTTP conditions will bubble up out of snooze and
possible land you in the debugger. This option also is useful during
development if you prefer an interactive debugger.")

(defparameter *catch-errors* t
  "If non-NIL, catch any error and explain them to the client.

If T (the default), *any* erroneous conditions that are a *fault of
the server and not of the user-agent*, including errors *and* HTTP
500-class conditions voluntarily signalled by the program logic, will
still result in a response to the user-agent.

To compose this response, Snooze will first try EXPLAIN-CONDITION to
\"politely\" explain the condition in a format accepted by the
user-agent, as indicated in the \"Accept:\" header. If that fails, the
error is presented very succintly to the client.

If the value is the keyword :VERBOSE, don't call EXPLAIN-CONDITION and
offer a plain text explanation with a full Lisp backtrace indicating
where the condition originated. This option is useful during
development.

If set to NIL, errors will bubble up out of snooze and possible land
you in the debugger. This option is also useful during development if
you prefer an interactive debugger")

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
content-designating components. The function may also return NIL.

The default value is SEARCH-FOR-EXTENSION-CONTENT-TYPE looks for the
first filename know extension inside the URI path. If found, it
returns a singleton list with that content type as the first value
and, as the second value, the URI path stripped of the extension
thus found.

Can be let-bound to modify the URI scheme used by a particular
server.")

(defun http-condition (status-code
                       &optional
                         format-control
                       &rest format-args)
  "Signal an HTTP condition with STATUS-CODE with with CL:ERROR."
  (error 'http-condition :status-code status-code
                         :format-control (or format-control (reason-for status-code))
                         :format-arguments format-args))

(defgeneric explain-condition (condition resource content-type )
  (:documentation "Explain CONDITION for RESOURCE in CONTENT-TYPE."))

(defgeneric uri-to-arguments (resource relative-uri)
  (:documentation
   "Extract arguments for RESOURCE from RELATIVE-URI.

RELATIVE-URI is a string, where everything but the part designating
RESOURCE has been kept untouched (and potentially URI-encoded)

Should return two values: a list of \"plain\" arguments and an
alist (*not* a plist) used as keyword arguments.

It's reasonable for user-written specializaions of this method to
error out with 400 (malformed) or 404 not found status codes.

This method is the inverse of ARGUMENTS-TO-URI")
  (:method ((resource resource-generic-function) relative-uri)
    "Default method of URI-TO-ARGUMENTS.

Converts the path section of RELATIVE-URI into \"plain\" arguments and
the query and fragment sections of URI into keyword
arguments. READ-FROM-STRING is used to convert every individual
argument's value. If an argument is unconvertible, an HTTP 400
condition of type UNCONVERTIBLE-ARGUMENT is signalled."
    (uri-to-arguments-1 resource relative-uri)))

(defgeneric arguments-to-uri (resource plain-args keyword-args)
  (:documentation
   "Generate an URI path string to fit RESOURCE.

PLAIN-ARGS and KEYWORD-ARGS are like the return values of
URI-TO-ARGUMENTS.

Should return a propertly escaped URI path that will display in the
address bar and/or be sent on future requests.

This method is the inverse of URI-TO-ARGUMENTS.")
  (:method ((resource resource-generic-function) plain-args keyword-args)
    "Default method of ARGUMENTS-TO-URI.

Tries to WRITE-TO-STRING (with *PRINT-CASE* set to :DOWNCASE) every
object, except for keywords, which are written without the leading
\":\" character. Afterwards the whole URI is escaped for invalid
sequences."
    (arguments-to-uri-1 resource plain-args keyword-args)))

(defgeneric read-for-resource (resource string)
  (:documentation
   "Like READ-FROM-STRING, but for RESOURCE.

Reads the object represented in STRING into a CL representation,
considering RESOURCE.")
  (:method ((resource resource-generic-function) string)
    "Defaut method for READ-FOR-RESOURCE.

Vaguely resembles READ-FROM-STRING, but will only read in numbers,
symbols or strings. Unqualified symbols are read in the package where
RESOURCE belongs, otherwise they must be package-qualified.  If a
symbol, package, qualified or not, does not exist, it is *not*
created.  Instead, an uninterned symbol of the intended name is
returned instead.

This means that:

    (loop for outgoing in '(cl:defun
                            :just-interned-this
                            and-this
                            #:uninterned)
          for readback = (read-for-resource res
                            (write-for-resource res outgoing))
          collect
          (list (eq outgoing readback)
                (string= (string outgoing)
                         (string readback))))


Returns ((T T) (T T) (T T) (NIL T))."
    (read-for-resource-1 resource string)))

(defgeneric write-for-resource (resource obj)
  (:documentation
   "Like WRITE-TO-STRING, but for RESOURCE.

Returns a string representing the object OBJ, considering RESOURCE.

The default implementation ")
  (:method ((resource resource-generic-function) string)
    "Defaut method for WRITE-FOR-RESOURCE.

Calls WRITE-TO-STRING on OBJECT with the current package set to the
package of the RESOURCE's symbol, except in the case that OBJECT is an
uninterned symbol, whereupon PRINC-TO-STRING is used on its downcased
name instead."
    (write-for-resource-1 resource string)))

(defun handle-request (uri &key
                             (method :get)
                             (accept "*/*")
                             (content-type "application/x-www-form-urlencoded"))
  "Dispatches an HTTP request for URI to the appropriate resource.

METHOD a keyword, string or symbol designating the HTTP method (or
\"verb\").

ACCEPT is a string in the format of the \"Accept:\" header.

IN-CONTENT-TYPE is a string in the format of the \"Content-Type\"
header in the request, used when METHOD is :POST or :PUT, in which
case it must be non-nil.

Returns three values CODE, PAYLOAD and OUT-CONTENT-TYPE, which should
be used by the application to craft a response to the request."
  (handle-request-1 uri method accept content-type))

(defvar *backend*)
(setf (documentation '*backend* 'variable)
      "Bound to a keyword identifying the server backend handling a request.
Examples of values to find here are :HUNCHENTOOT or :CLACK.")

(defgeneric backend-payload (backend type)
  (:documentation
   "Ask BACKEND to return the current HTTP request's payload as TYPE.

BACKEND is a value suitable for *BACKEND* (which see).

Type is an instance of SNOOZE-TYPES:CONTENT."))

(defun payload-as-string (&optional (backend *backend*))
  "Return the current HTTP request's payload as a string.

BACKEND defaults to *BACKEND*"
  (backend-payload backend (make-instance 'snooze-types:text)))



;;; Clack integration
;;;
(defvar *clack-request-env*)

(setf (documentation '*clack-request-env* 'variable)
      "Bound in function made by MAKE-CLACK-APP to Clack environment.")

(defun make-clack-app (&optional bindings)
  "Make a basic Clack app that calls HANDLE-REQUEST.

Pass this to CLACK:CLACKUP.

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
                        (apply #'handle-request (getf env :request-uri)
                               (loop for (k v) on (list
                                                   :method (getf env :request-method)
                                                   :accept (gethash "accept" (getf env :headers))
                                                   :content-type (getf env :content-type))
                                     by #'cddr
                                     when v collect k and collect v))
                      `(,status-code
                        (:content-type ,payload-ct)
                        (,payload)))))))

(defmethod backend-payload ((backend (eql :clack)) (type snooze-types:text))
  (let* ((len (getf *clack-request-env* :content-length))
         (str (make-string len)))
    (read-sequence str (getf *clack-request-env* :raw-body))
    str))



;;; Direct hunchentoot integration
;;;
(defun make-hunchentoot-app (&optional bindings)
  "Make a basic Hunchentoot dispatcher that calls HANDLE-REQUEST.

Add this to HUNCHENTOOT:*DISPATCH-TABLE*, possibly after some static
file dispatcher or other dispatcher you wish to kick in before
Snooze. Then start an HUNCHENTOOT:EASY-ACCEPTOR at whichever port you
choose.

BINDINGS is an alist of (SYMBOL . VALUE) which is are also
dynamically-bound around HANDLE-REQUEST. You can use it to pass values
of special variables that affect Snooze, like *HOME-RESOURCE*,
*RESOURCES-FUNCTION*, *RESOURCE-NAME-FUNCTION*, or
*URI-CONTENT-TYPES-FUNCTION*."
  (let ((request-uri-fn (read-from-string "hunchentoot:request-uri"))
        (header-in-fn (read-from-string "hunchentoot:header-in"))
        (request-method-fn (read-from-string "hunchentoot:request-method"))
        (set-return-code-fn (read-from-string "(setf hunchentoot:return-code*)"))
        (set-content-type-fn (read-from-string "(setf hunchentoot:content-type*)")))
    (lambda (request)
      (lambda ()
        (let ((*backend* :hunchentoot))
          (progv
              (mapcar #'car bindings)
              (mapcar #'cdr bindings)
            (multiple-value-bind (status-code payload payload-ct)
                (apply #'handle-request
                       (funcall request-uri-fn request)
                       (loop for (k v) on (list 
                                           :method (funcall request-method-fn request)
                                           :accept (funcall header-in-fn :accept request)
                                           :content-type (funcall header-in-fn :content-type request))
                             by #'cddr
                             when v collect k and collect v))
              (funcall (fdefinition set-return-code-fn) status-code)
              (funcall (fdefinition set-content-type-fn) payload-ct)
              (or payload ""))))))))

(defmethod backend-payload ((backend (eql :hunchentoot)) (type snooze-types:text))
  (let ((probe (funcall (read-from-string "hunchentoot:raw-post-data"))))
    (assert (stringp probe) nil "Asked for a string, but request carries a ~a" (type-of probe))
    probe))
