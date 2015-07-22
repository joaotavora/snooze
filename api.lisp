;;; User facing API.
;;;
;;; Every external symbol here 
;;;
(in-package #:snooze)

(defclass snooze-server ()
  ((route-packages
    :initform (error "ROUTE-PACKAGES is a mandatory initarg")
    :initarg :route-packages
    :accessor route-packages
    :documentation
    "A list of packages to look for routes.
     Specifically, packages containing generic functions whose names
     match the names of REST resources. ")
   (resource-name-regexp
    :initform "/([^/.]+)" :initarg :resource-name-regexp
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
    "What to do if resource name extracted from URI doesn't match.
     If NIL, issue a 404. Otherwise let the backend of the
     SNOOZE-SERVER object handle the request")
   (home-resource
    :initform nil :initarg :home-resource
    :accessor home-resource
    :documentation
    "Default \"home\" resource, served when the requested URL is \"bare\".
     Value can be a string or a function designator.")
   (backend
    :reader backend
    :documentation "The backend implementation object"))
  (:documentation "HTTP server for handling RESTful routes"))

(defun start (server)
  "Start the Snooze REST server in SERVER"
  (snooze-backend:start server (backend server)))

(defun stop (server)
  "Stop the Snooze REST server in SERVER"
  (snooze-backend:stop server (backend server)))

(defun started-p (server)
  (snooze-backend:started-p server (backend server)))


;;; Advanced
;;; 
(defgeneric convert-arguments (server resource actual-arguments)
  (:method (server resource args)
    (declare (ignore server resource))
    (mapcar #'(lambda (arg)
                (let ((probe (let ((*read-eval* nil))
                                 (ignore-errors
                                  (read-from-string arg)))))
                  (if (numberp probe) probe arg)))
            args))
  (:documentation
   "In the context of SERVER, make ACTUAL-ARGUMENTS fit RESOURCE.
Should return a list equal to ACTUAL-ARGUMENTS, which is a list of
strings, but where some strings have been converted to other types.
The default method tries to convert every arguments to a number."))

(defgeneric expand-content-type (server resource content-type-class)
  (:method (server resource content-type-class)
    (declare (ignore server resource))
    (list content-type-class))
  (:method (server resource (content-type-class supertype-metaclass))
    (reduce #'append
            (mapcar
             (alexandria:curry #'expand-content-type server resource)
                    (closer-mop:class-direct-subclasses content-type-class))))
  (:documentation
   "For SERVER and RESOURCE, expand CONTENT-TYPE-CLASS.
RESOURCE is a generic function designating a REST resource. The goal
of this function is to expand wildcard content-types like
\"application/*\" into specific content-types to probe RESOURCE with.
The default implementation is very inneficient, it ignores resource
and completely expands the wildcard content-type."))


;;; Route definition
;;;
(defmacro defresource (name lambda-list &rest options)
  (let* ((genurl-form)
         (defgeneric-args
           (loop for option in options
                 for routep = (eq :route (car option))
                 for (qualifiers spec-list body)
                   = (and routep
                          (multiple-value-list
                           (parse-defroute-args (cdr option))))
                 for verb-spec = (and routep
                                      (verb-spec-or-lose (first spec-list)))
                 for type-spec = (and routep
                                      (content-type-spec-or-lose (second spec-list)
                                                                 (second verb-spec)))
                 
                 if routep
                   collect `(:method
                              ,@qualifiers
                              (,verb-spec ,type-spec ,@(nthcdr 2 spec-list))
                              ,@body)
                 else if (eq :genurl (car option))
                        do (setq genurl-form
                                 (make-genurl-form (second option) name
                                                   (nthcdr 2 lambda-list)))
                 else
                   collect option))
         (simplified-lambda-list (mapcar #'(lambda (argspec)
                                             (ensure-atom argspec))
                                         lambda-list)))
    `(progn
       (defgeneric ,name ,simplified-lambda-list
         ,@defgeneric-args)
       (defmethod no-applicable-method ((f (eql (function ,name))) &rest args)
         (error 'no-such-route
                :format-control "No applicable route ~%  ~a~%when called with args ~%  ~a" 
                :format-arguments (list f args)))
       (defmethod check-arguments ((f (eql (function ,name))) actual-arguments)
         (apply 
          (lambda ,lambda-list
            (declare (ignore ,@(remove-if #'(lambda (sym)
                                              (eq #\& (aref (symbol-name sym) 0)))
                                          simplified-lambda-list))))
          actual-arguments))
       (defmethod resource-p ((f (eql (function ,name)))) t)
       ,@(if genurl-form `(,genurl-form)))))

(defmacro defroute (name &body args)
  (let* (;; find the qualifiers and lambda list
         ;; 
         (first-parse
           (multiple-value-list
            (parse-defroute-args args)))
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


;;; Conditions
;;
(define-condition http-condition (simple-condition)
  ((status-code :initarg :status-code :initform (error "Must supply a HTTP status code.")
                :accessor status-code))
  (:default-initargs :format-control "HTTP condition"))

(define-condition |404| (http-condition) () (:default-initargs :status-code 404))

(defun http-error (status-code
                   &optional (format-control nil format-control-supplied-p)
                             (format-args nil format-args-supplied-p))
  (apply #'error 'http-condition :status-code status-code
         `(,@(if format-args-supplied-p
                 `(:format-arguments ,format-args))
           ,@(if format-control-supplied-p
                 `(:format-control ,format-control)))))

(define-condition no-such-resource (|404|) ()
  (:default-initargs
   :format-control "Resource does not exist"))

(define-condition invalid-resource-arguments (|404|) ()
  (:default-initargs
   :format-control "Resource exists but invalid arguments passed"))

(define-condition no-such-route (|404|) ()
  (:default-initargs
   :format-control "Resource exists but no such route"))

(define-condition no-matching-content-types (no-such-route)
  ((accepted-classes :initarg :accepted-classes :accessor accepted-classes))
  (:default-initargs
   :format-control "Resource exists but client's \"Accept:\" header too restrictive"))

(defgeneric explain-condition (server c)
  (:documentation
   "In the context of SERVER, explain exceptional condition C to client."))

(defmethod explain-condition (server (c http-condition))
  (declare (ignore server))
  (format nil "~?" (simple-condition-format-control c)
          (simple-condition-format-arguments c)))

(defmethod explain-condition (server (c error))
  (declare (ignore server))
  "Something nasty happened")

(defparameter *always-explain-conditions* nil
  "If non-nil, always catch and explain HTTP-CONDITION conditions.
This is even if the backend is configured not to catch errors.")


;;; More stuff needed
;;;
(defun request-body ()
  (snooze-backend:request-body (backend snooze-backend:*current-server*)))


;;; Internal
;;; 
(defmethod initialize-instance :after ((server snooze-server)
                                       &rest args
                                       &key
                                         (backend :hunchentoot)
                                         route-packages
                                       &allow-other-keys)
  (assert (listp route-packages) nil "ROUTE-PACKAGES must be a list")
  (loop for package in route-packages
        do (assert (find-package package) nil "~a in ROUTE-PACKAGES is not a package!"
                   package))
  (setf (slot-value server 'backend)
        (apply #'make-instance (snooze-backend:backend-class backend) :server server
               (loop for (k v) on args by #'cddr
                     unless (member k '(:route-packages :resource-name-regexp
                                        :fall-through-p :home-resource))
                       collect k and collect v))))

(defmethod print-object ((c http-condition) s)
  (format s "HTTP ~a: ~?" (status-code c)
            (simple-condition-format-control c)
            (simple-condition-format-arguments c)))




