(in-package #:resting)


;;; API
;;;
(defclass rest-acceptor (hunchentoot:acceptor)
  ((route-packages
    :initform (list *package*)
    :initarg :route-packages :accessor route-packages
    :documentation
    "A list of packages to look for routes.
     Specifically, packages containing generic functions whose names
     match the names of REST resources. ")
   (resource-name-regexp
    :initform "/([^/]+)" :initarg :resource-name-regexp
    :accessor resource-name-regexp
    :documentation
    "A CL-PPCRE regular expression to match a REST resource's name.
     When applied to an URI path should match a REST resource's
     name. The remainder of the URI are the actual arguments for the
     HTTP verbs that operate on that resource. The first regexp
     capturing group, if any, is used for the resource's name.")
   (fall-through-p
    :initform nil :initarg :fall-through-p
    :accessor fall-through-p
    :documentation
    "What to do if resource name extracted from URI doesn't match."))
  (:documentation "An acceptor for RESTful routes"))

(defgeneric explain-condition (acceptor c)
  (:documentation
   "In the context of ACCEPTOR, explain exceptional condition C to client.
New methods added can/should set HUNCHENTOOT:CONTENT-TYPE* on the
current reply."))

(defgeneric convert-arguments (acceptor resource actual-arguments)
  (:method (acceptor resource args)
    (declare (ignore acceptor resource))
    (mapcar #'(lambda (arg)
                (let ((probe (let ((*read-eval* nil))
                                 (ignore-errors
                                  (read-from-string arg)))))
                  (if (numberp probe) probe arg)))
            args))
  (:documentation
   "In the context of ACCEPTOR, make ACTUAL-ARGUMENTS fit RESOURCE.
Should return a list equal to ACTUAL-ARGUMENTS, which is a list of
strings, but where some strings have been converted to other types.
The default method tries to convert every arguments to a number."))

(defgeneric expand-content-type (acceptor resource content-type-class)
  (:method (acceptor resource content-type-class)
    (declare (ignore acceptor resource))
    (list content-type-class))
  (:method (acceptor resource (content-type-class supertype-metaclass))
    (reduce #'append
            (mapcar
             (alexandria:curry #'expand-content-type acceptor resource)
                    (closer-mop:class-direct-subclasses content-type-class))))
  (:documentation
   "For ACCEPTOR and RESOURCE, expand CONTENT-TYPE-CLASS.
Resource is a generic function designating a REST resource. The goal
of this function is to expand wildcard content-types like
\"application/*\" into specific content-types to probe RESOURCE with.
The default implementation is very inneficient, it ignores resource
and completely expands the wildcard content-type."))


;;; Verbs
;;;
;;; "Sending" and "Receiving" are always from the server's
;;; perspective. Hence GET is "sending" and POST and PUT are
;;; "receiving".
;;; 
(defpackage :resting-verbs (:use) (:export #:http-verb #:get #:post #:put #:delete
                                           #:content-verb
                                           #:receiving-verb
                                           #:sending-verb))
(in-package :resting-verbs)

(cl:defclass http-verb () ())

(cl:defclass delete (http-verb) ())

(cl:defclass content-verb (http-verb) ())

(cl:defclass receiving-verb (content-verb) ())
(cl:defclass sending-verb   (content-verb) ())

(cl:defclass post (receiving-verb) ())
(cl:defclass put  (receiving-verb) ())

(cl:defclass get (sending-verb) ())

;;; Content-types
;;;
;;; Note that in GET requests we are only interested in the request's
;;; "Accept" header, since GET never have useful bodies (1) and as
;;; such don't have "Content-Type".
;;;
;;; [1]: http://stackoverflow.com/questions/978061/http-get-with-request-body
;;;
;;; So, for GET requests, we the hierarchy is actually inverse.
;;;
(cl:in-package :resting)
;; (delete-package :resting-types)
(defpackage :resting-types (:use) (:export #:content))

(eval-when (:compile-toplevel :load-toplevel :execute)

  (defclass supertype-metaclass (standard-class) ())

  (defmethod closer-mop:validate-superclass ((class supertype-metaclass)
                                             (superclass standard-class))
    t)

  (defmethod closer-mop:validate-superclass ((superclass standard-class)
                                             (class supertype-metaclass))
    t)

  (defclass resting-types:content ()
    ((content-body :initarg :content-body
                   :accessor content-body
                   :documentation "A sequence containing the body of the
                     request that the route decided to handle."))
    (:metaclass supertype-metaclass))
  
  (defun intern-safe (designator package)
    (intern (string-upcase designator) package))
  (defun send-any-symbol (supertype)
    (intern (string-upcase (format nil "SEND-ANY-~a" supertype))
            :resting-types))
  (defun scan-to-strings* (regex string)
    (coerce (nth-value 1
                       (cl-ppcre:scan-to-strings regex
                                                 string))
            'list)))



(defmacro define-content (type-designator
                          &optional (supertype-designator
                                     (first (scan-to-strings* "([^/]+)" type-designator))))
  (let* ((type (intern-safe type-designator :resting-types))
         (supertype (intern-safe supertype-designator :resting-types)))
    `(progn
       (unless (find-class ',supertype nil)
         (defclass ,supertype (resting-types:content) ()
           (:metaclass supertype-metaclass) ))
       (defclass ,type (,supertype) ())
       (eval-when (:compile-toplevel :load-toplevel :execute)
         (export '(,type ,supertype) :resting-types)))))

(defmacro define-known-content-types ()
  `(progn
     ,@(loop for (type-spec . nil) in hunchentoot::*mime-type-list*
             for matches = (nth-value 1 (cl-ppcre:scan-to-strings "(.*/.*)(?:;.*)?" type-spec))
             for type = (and matches (aref matches 0))
             when type
               collect `(define-content ,type))))


;;; Define some things at compile-time
;;;
(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-known-content-types))


;;; DEFROUTE Helpers
;;; 
(defun find-class-1 (sym)
  "Like CL:FIND-CLASS but don't error and return SYM or nil"
  (when (find-class sym nil)
    sym))

(defun verb-spec-or-lose (verb-spec)
  "Convert VERB-SPEC into something `defmethod' can grok."
  (labels ((verb-designator-to-verb (designator)
             (or (and (eq designator 't)
                      (progn
                        (alexandria:simple-style-warning
                         "Coercing verb-designating type T in ~a to ~s"
                              verb-spec 'resting-verbs:http-verb)
                        'resting-verbs:http-verb))
                 (find-class-1 (intern (string-upcase designator)
                                       :resting-verbs))
                 (error "Sorry, don't know the HTTP verb ~a"
                        (string-upcase designator)))))
    (cond ((and verb-spec
                (listp verb-spec))
           (list (first verb-spec) (verb-designator-to-verb (second verb-spec))))
          ((or (keywordp verb-spec)
               (stringp verb-spec))
           (list 'verb (verb-designator-to-verb verb-spec)))
          (verb-spec
           (list verb-spec 'resting-verbs:http-verb)))))

(defun find-content-class (designator)
  "Return DESIGNATOR as content-type class-defining symbol or nil."
  (or (and (eq designator t)
           (progn
             (alexandria:simple-style-warning
              "Coercing content-designating type designator T to ~s"
              'resting-types:content)
             (find-class 'resting-types:content)))
      (find-class (intern (string-upcase designator) :resting-types) nil)
      (and (string= designator "*/*") (find-class 'resting-types:content))
      (let* ((matches (nth-value 1
                                 (cl-ppcre:scan-to-strings
                                  "([^/]+)/\\*"
                                  (string-upcase designator))))
             (supertype-designator (and matches
                                        (aref matches 0))))
        (find-class
         (intern (string-upcase supertype-designator) :resting-types)
         nil))))

(defun content-type-spec-or-lose-1 (type-spec)
  (labels ((type-designator-to-type (designator)
             (let ((class (find-content-class designator)))
               (if class (class-name class)
                   (error "Sorry, don't know the content-type ~a" type-spec)))))
    (cond ((and type-spec
                (listp type-spec))
           (list (first type-spec) (type-designator-to-type (second type-spec))))
          ((or (keywordp type-spec)
               (stringp type-spec))
           (list 'type (type-designator-to-type type-spec)))
          (type-spec
           (list type-spec (type-designator-to-type t))))))

(defun content-type-spec-or-lose (type-spec verb)
  (cond ((subtypep verb 'resting-verbs:content-verb)
         (content-type-spec-or-lose-1 type-spec))
        ((and type-spec (listp type-spec))
         (assert (eq t (second type-spec))
                 nil
                 "For verb ~a, no specializations on Content-Type are allowed"
                 verb)
         type-spec)
        (t
         (list type-spec t))))

(defun ensure-atom (thing)
  (if (listp thing)
      (ensure-atom (first thing))
      thing))

(defgeneric check-arguments (function actual-arguments))

(defgeneric resource-p (function))

(defmethod check-arguments (function actual-arguments)
  (declare (ignore function actual-arguments)))


;; Conditions
;;

(defmacro define-error (code &optional default-msg)
  (let ((sym (intern (princ-to-string code))))
    `(progn
       (define-condition ,sym (http-condition)
         () (:default-initargs :code ,code))
       (defun ,sym (&optional (format-control ,default-msg) format-args)
         (error ',sym :format-control format-control :format-arguments format-args))
       (eval-when (:compile-toplevel :load-toplevel :execute)
         (export ',sym)))))

(define-condition http-condition (simple-error)
  ((status-code :initarg :code :initform (error "Must supply a HTTP status code.")
                :accessor code))
  (:default-initargs :format-control "An HTTP error has occured"))

(define-condition no-such-route (http-condition)
  () (:default-initargs :code 404))

(define-condition no-matching-content-types (no-such-route)
  ((accepted-classes :initarg :accepted-classes :accessor accepted-classes))
  (:default-initargs :format-control "\"Accept:\" header was too specific"))

(define-error 404 "Not Found")
;; FIXME define more

(defmethod print-object ((c http-condition) s)
  (format s "HTTP ~a: ~?" (code c)
            (simple-condition-format-control c)
            (simple-condition-format-arguments c)))

(defmethod explain-condition (acceptor c)
  (declare (ignore acceptor))
  (setf (hunchentoot:content-type*) "text/plain")
  (format nil "~?" (simple-condition-format-control c)
          (simple-condition-format-arguments c)))


;; DEFRESOURCE and DEFROUTE macros
;;
(defmacro defresource (name lambda-list &body body)
  `(progn
     (defgeneric ,name ,lambda-list ,@body)
     (defmethod no-applicable-method ((f (eql (function ,name))) &rest args)
       (error 'no-such-route
              :format-control "No applicable route ~%  ~a~%when called with args ~%  ~a" 
              :format-arguments (list f args)))
     (defmethod check-arguments ((f (eql (function ,name))) actual-arguments)
       (apply 
        (lambda ,lambda-list
          (declare (ignore ,@(remove-if #'(lambda (sym)
                                            (eq #\& (aref (symbol-name sym) 0)))
                                        lambda-list))))
        actual-arguments))
     (defmethod resource-p ((f (eql (function ,name)))) t)))

(defmacro defroute (name &body args)
  (let* (;; find the qualifiers and lambda list
         ;; 
         (first-parse
           (loop for args on args
                 if (listp (first args))
                   return (list qualifiers (first args) (cdr args))
                 else
                   collect (first args) into qualifiers))
         (qualifiers (first first-parse))
         (lambda-list (second first-parse))
         (body (third first-parse))
         ;; now parse body
         ;; 
         (parsed-body (multiple-value-list (alexandria:parse-body body)))
         (remaining (first parsed-body))
         (declarations (second parsed-body))
         (docstring (third parsed-body))
         ;; Add syntactic sugar for the first two specializers in the
         ;; lambda list
         ;; 
         (verb-spec (verb-spec-or-lose (first lambda-list)))
         (type-spec (content-type-spec-or-lose (second lambda-list) (second verb-spec)))
         (proper-lambda-list
           `(,verb-spec ,type-spec ,@(nthcdr 2 lambda-list)))
         (simplified-lambda-list
           (mapcar #'ensure-atom proper-lambda-list)))
    `(progn
       (unless (and (fboundp ',name)
                    (resource-p (function ,name)))
         (defresource ,name ,simplified-lambda-list))
       (defmethod ,name ,@qualifiers
         ,proper-lambda-list
         ,@(if docstring `(,docstring))
         ,@declarations
         ,@remaining))))


;;; Helpers for our HUNCHENTOOT:ACCEPTOR-DISPATCH-REQUEST method
;;;
(defun parse-args-in-uri (args-string)
  (let* ((required-and-query-and-fragment
           (scan-to-strings*
            "/?([^?]+)(?:\\?([^#]+))?(?:#(.*))?$"
            args-string))
         (required-args (cl-ppcre:split "/" (first required-and-query-and-fragment)))
         (keyword-args (loop for maybe-pair in (cl-ppcre:split "[;&]" (second required-and-query-and-fragment))
                             for (key-name value) = (scan-to-strings* "(.*)=(.*)" maybe-pair)
                             when (and key-name value)
                               append (list (intern (string-upcase key-name) :keyword)
                                            value)))
         (fragment (third required-and-query-and-fragment)))
    (append required-args
            keyword-args
            (when fragment
              (list 'resting:fragment fragment)))))

(defun parse-uri (uri acceptor)
  "Parse URI and return actual arguments for a lambda list."
  ;; <scheme name> : <hierarchical part> [ ? <query> ] [ # <fragment> ]
  (let* ((resource-name-regexp (resource-name-regexp acceptor))
         (scan-results (multiple-value-list (cl-ppcre:scan resource-name-regexp uri)))
         (method-name
           (apply #'subseq uri
                  (if (plusp (length (third scan-results)))
                      (list (aref (third scan-results) 0) (aref (fourth scan-results) 0))
                      (list (first scan-results) (second scan-results)))))
         (actual-arguments (parse-args-in-uri (subseq uri (second scan-results)))))
    (values (loop for package in (route-packages acceptor)
                    thereis (find-symbol (string-upcase
                                          method-name) package))
            actual-arguments)))

(defun parse-accept-header (string acceptor resource)
  "Return a list of class objects designating " 
  (loop for media-range-and-params in (cl-ppcre:split "\\s*,\\s*" string)
        for media-range = (first (scan-to-strings* "([^;]*)"
                                                   media-range-and-params))
        for class = (find-content-class media-range)
        when class
          append (expand-content-type acceptor resource class)))

(defun arglist-compatible-p (method args)
  (handler-case
      (progn
        (check-arguments (symbol-function method)
                         (append
                          (list 'dummy 'dummy)
                          args))
        t)
    (error () nil)))

(defun parse-content-type-header (string)
  "Return a symbol designating a RESTING-SEND-TYPE object."
  (find-content-class string))

(defmethod hunchentoot:acceptor-dispatch-request ((acceptor rest-acceptor) request)
  (let* ((content-class (parse-content-type-header (hunchentoot:header-in :content-type request)))
         (verb-designator (or (find-class-1
                               (intern (string-upcase (hunchentoot:request-method request))
                                       :resting-verbs))
                              (error "Can't find HTTP verb designator for request ~a!" request)))
         ;; FIXME: maybe use singletons here
         (verb (and verb-designator
                    (make-instance verb-designator))) 
         (method-and-args
           (multiple-value-list
            (parse-uri (hunchentoot:request-uri request) acceptor)))
         (resource (first method-and-args))
         (route-arguments
           (convert-arguments acceptor resource (second method-and-args)))
         (accepted-classes (parse-accept-header (hunchentoot:header-in :accept request)
                                                acceptor
                                                resource)))
    (handler-bind ((http-condition
                     #'(lambda (c)
                         ;; FIXME: make this a restart
                         (when hunchentoot:*catch-errors-p*
                           (setf (hunchentoot:return-code*) (code c))
                           (hunchentoot:abort-request-handler
                            (explain-condition acceptor c))))))
      (cond ((not resource)
             (if (fall-through-p acceptor)
                 (call-next-method)
                 (error 'no-such-route
                        :format-control
                        "So sorry, but that URI doesn't match any REST resources")))
            ((not (arglist-compatible-p resource
                                        (second method-and-args)))
             (error 'no-such-route
                    :format-control
                    "Too many, too few, or unsupported query arguments for REST resource ~a"
                    :format-arguments
                    (list resource)))
            (t
             (etypecase verb
               ;; For the Accept: header
               (resting-verbs:sending-verb
                (let ((try-list accepted-classes)
                      (retval))
                  (loop do (unless try-list
                             (error 'no-matching-content-types
                                    :accepted-classes accepted-classes
                                    :format-control
                                    "No matching routes for verb~%  ~a~%on resource~%  ~a~%and accept list~%  ~a"
                                    :format-arguments
                                    (list verb resource
                                          (mapcar #'class-name accepted-classes))))
                          thereis
                          (block try-again
                            (handler-bind ((no-such-route
                                             #'(lambda (c)
                                                 (declare (ignore c))
                                                 (return-from try-again nil))))
                              ;; autosetting the reply's content-type
                              ;; before the method call gives the
                              ;; route a chance to override it.
                              ;;
                              (let ((content-type (pop try-list)))
                                (setf (hunchentoot:content-type*)
                                      (string (class-name content-type)))
                                (setq retval
                                      (apply resource
                                             verb
                                             ;; FIXME: maybe use singletons here
                                             (make-instance (class-name content-type))
                                             route-arguments))
                                t))))
                  retval))
               (resting-verbs:receiving-verb
                (apply resource
                       verb
                       (make-instance (class-name content-class) 
                                      :content-body
                                      (hunchentoot:raw-post-data :request request))
                       route-arguments))))))))

(defmethod hunchentoot:acceptor-status-message ((acceptor rest-acceptor) status-code &key
                                                &allow-other-keys)
  (declare (ignore status-code))
  (call-next-method))


