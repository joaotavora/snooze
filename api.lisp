;;; User facing API.
;;;
;;; Every external symbol here 
;;;
(in-package #:snooze)


;;; Route definition
;;;
(defmacro defresource (name lambda-list &rest options)
  (let* ((genpath-form)
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
                 else if (eq :genpath (car option))
                        do (setq genpath-form
                                 (make-genpath-form (second option) name
                                                   (nthcdr 2 lambda-list)))
                 else
                   collect option))
         (simplified-lambda-list (mapcar #'(lambda (argspec)
                                             (ensure-atom argspec))
                                         lambda-list)))
    `(progn
       ,@(if genpath-form `(,genpath-form))
       (defgeneric ,name ,simplified-lambda-list
         (:generic-function-class resource-generic-function)
         ,@defgeneric-args))))

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
       (unless (find-resource ',name)
         (defresource ,name ,simplified-lambda-list))
       (defmethod ,name ,@qualifiers
         ,proper-lambda-list
         ,@(if docstring `(,docstring))
         ,@declarations
         ,@remaining))))


;;; Conditions
;;
(defparameter *catch-http-conditions* t)
(defparameter *catch-errors* nil)

(define-condition http-condition (simple-condition)
  ((status-code :initarg :status-code :initform (error "Must supply a HTTP status code.")
                :reader status-code))
  (:default-initargs :format-control "HTTP condition"))

(define-condition http-error (http-condition simple-error) ()
  (:default-initargs
   :format-control "HTTP Internal Server Error"
   :status-code 500))

(defmethod initialize-instance :after ((e http-error) &key)
  (assert (<= 500 (status-code e) 599) nil
          "An HTTP error must have a status code between 500 and 599"))

(defun http-condition (status-code
                   &optional (format-control nil format-control-supplied-p)
                             (format-args nil format-args-supplied-p))
  (apply #'error 'http-condition :status-code status-code
         `(,@(if format-args-supplied-p
                 `(:format-arguments ,format-args))
           ,@(if format-control-supplied-p
                 `(:format-control ,format-control)))))



(define-condition no-such-resource (http-condition) ()
  (:default-initargs
   :status-code 404
   :format-control "Resource does not exist"))

(define-condition invalid-resource-arguments (http-condition) ()
  (:default-initargs
   :status-code 400
   :format-control "Resource exists but invalid arguments passed"))

(define-condition  unsupported-content-type (http-error) ()
  (:default-initargs
   :status-code 501
   :format-control "Resource exists but invalid arguments passed"))

(define-condition no-such-route (http-condition) ()
  (:default-initargs
   :format-control "Resource exists but no such route"))

(defgeneric explain-condition (condition resource content-type )
  (:documentation "Explain CONDITION for RESOURCE in CONTENT-TYPE.")
  (:method (condition resource (content-type (eql 'failsafe)))
    (declare (ignore resource))
    (format nil "~a" condition))
  (:method (condition resource (content-type (eql 'full-backtrace)))
    (declare (ignore resource))
    (format nil "Your SNOOZE was bitten by:~&~a"
            (with-output-to-string (s)
              (uiop/image:print-condition-backtrace condition :stream s)))))


;;; Advanced
;;;
(defgeneric convert-arguments (resource plain-arguments keyword-arguments)
  (:method (resource plain-arguments keyword-arguments)
    (declare (ignore resource))
    (flet ((probe (value)
             (or (let ((*read-eval* nil))
                   (ignore-errors
                    (read-from-string value)))
                 value)))
    (append
     (mapcar #'probe plain-arguments)
     (loop for (key value) on keyword-arguments by #'cddr
           collect key
           collect (probe value)))))
  (:documentation
   "In the context of SERVER, make ACTUAL-ARGUMENTS fit RESOURCE.
Should return a list of the same length as ACTUAL-ARGUMENTS, which is
a list of strings, but where some strings have been converted to other
types.  The default method tries to convert every arguments to a
number."))


(defun matching-content-type-or-lose (resource verb args try-list)
  "Check RESOURCE for route matching VERB, TRY-LIST and ARGS.
TRY-LIST, a list of subclasses of SNOOZE-TYPES:CONTENT, is iterated.
The first subclass for which RESOURCE has a matching specializer is
used to create an instance, which is returned. If none is found error
out with NO-SUCH-ROUTE."
  (or (some (lambda (maybe)
              (when (gf-primary-method-specializer
                     resource
                     (list* verb maybe args)
                     1)
                maybe))
            (mapcar #'make-instance try-list))
      (error 'no-such-route
             :status-code (if (destructive-p verb)
                              415 ; unsupported media type
                              406 ; not acceptable
                              ))))

(defun call-brutally-explaining-conditions (fn)
  (let (code condition)
    (flet ((explain (how)
             (throw 'response
               (values code
                       (explain-condition condition *resource* how)
                       'text/plain))))
      (restart-case (handler-bind ((error
                                     (lambda (e)
                                       (setq code 500 condition e)
                                       (cond ((eq *catch-errors* :backtrace)
                                              (invoke-restart 'explain-with-backtrace))
                                             (*catch-errors*
                                              (invoke-restart 'failsafe-explain)))))
                                   (http-condition
                                     (lambda (c)
                                       (setq code (status-code c) condition c)
                                       (cond ((eq *catch-http-conditions* :backtrace)
                                              (invoke-restart 'explain-with-backtrace))))))
                      (funcall fn))
        (explain-with-backtrace () :report
          (lambda (s) (format s "Explain ~a condition with full backtrace" code))
          (explain 'full-backtrace))
        (failsafe-explain () :report
          (lambda (s) (format s "Explain ~a condition very succintly" code))
          (explain 'failsafe))))))

(defun call-politely-explaining-conditions (client-accepts fn)
  (let (code
        condition
        accepted-type)
    (labels ((accepted-type (condition)
               (some (lambda (wanted)
                       (when (gf-primary-method-specializer
                              #'explain-condition
                              (list condition *resource* wanted)
                              1)
                         wanted))
                     (mapcar #'make-instance client-accepts)))
             (explain ()
               (throw 'response
                 (values code
                         (explain-condition condition *resource* accepted-type)
                         accepted-type))))
      (restart-case 
          (handler-bind ((condition
                           (lambda (c)
                             (setq condition c
                                   accepted-type (accepted-type condition))
                             (unless accepted-type
                               (error "Cannot politely explain~%~a~%to client, who only accepts~%~a"
                                      c client-accepts))))
                         (http-condition
                           (lambda (c)
                             (setq code (status-code c))
                             (when (and *catch-http-conditions*
                                        (not (eq *catch-http-conditions* :backtrace)))
                               (invoke-restart 'politely-explain))))
                         (error
                           (lambda (e)
                             (declare (ignore e))
                             (setq code 501)
                             (when (and *catch-errors*
                                        (not (eq *catch-errors* :backtrace)))
                               (invoke-restart 'politely-explain)))))
            (funcall fn))
        (politely-explain ()
          :report (lambda (s)
                    (format s "Politely explain to client in ~a"
                            accepted-type))
          :test (lambda (c) (declare (ignore c)) accepted-type)
          (explain))
        (auto-catch ()
          :report (lambda (s)
                    (format s "Start catching ~a automatically"
                            (if (typep condition 'http-condition)
                                "HTTP conditions" "errors")))
          :test (lambda (c)
                  (if (typep c 'http-condition)
                      (not *catch-http-conditions*)
                      (not *catch-errors*)))
          (if (typep condition 'http-condition)
              (setq *catch-http-conditions* t)
              (setq *catch-errors* t))
          (if (find-restart 'politely-explain)
              (explain)
              (if (find-restart 'failsafe-explain)
                  (invoke-restart 'failsafe-explain))))))))

(defmacro brutally-explaining-conditions (() &body body)
  "Explain conditions in BODY in a failsafe way.
Honours the :BACKTRACE option to *CATCH-ERRORS* and *CATCH-HTTP-CONDITIONS*."
  `(call-brutally-explaining-conditions (lambda () ,@body)))

(defmacro politely-explaining-conditions ((client-accepts) &body body)
  "Explain conditions in BODY taking the client accepts into account.
Honours *CATCH-ERRORS* and *CATCH-HTTP-CONDITIONS*"
  `(call-politely-explaining-conditions ,client-accepts (lambda () ,@body)))

(defparameter *allow-extension-as-accept* t)

(defvar *resource*
  "Bound early in HANDLE-REQUEST to nil or to a RESOURCE.
Used by POLITELY-EXPLAINING-CONDITIONS and
BRUTALLY-EXPLAINING-CONDITIONS to pass a resource to
EXPLAIN-CONDITION.")
(defun handle-request (uri
                       &key
                         (method :get)
                         (accept "*/*")
                         (content-type "application/x-www-form-urlencoded"))
  (catch 'response
    (let (*resource* plain-args keyword-args content-class-in-uri)
      (brutally-explaining-conditions ()
        (multiple-value-setq (*resource* plain-args keyword-args content-class-in-uri)
          (parse-resource (puri:parse-uri uri)))
        (let* ((verb (find-verb-or-lose method))
               (client-accepted-content-types
                 `(,@(if (and content-class-in-uri
                              *allow-extension-as-accept*)
                         (list content-class-in-uri))
                   ,@(or (content-classes-in-accept-string accept)
                         (list (find-content-class 'snooze-types:text/plain))))))
          (politely-explaining-conditions (client-accepted-content-types)
            (unless *resource*
              (error 'no-such-resource
                     :format-control
                     "So sorry, but that URI doesn't match any REST resources"))
            (let ((converted-arguments (convert-arguments *resource* plain-args keyword-args)))
              (unless (arglist-compatible-p *resource* converted-arguments)
                (error 'invalid-resource-arguments
                       :format-control
                       "Too many, too few, or unsupported query arguments for REST resource ~a"
                       :format-arguments
                       (list *resource*)))
              (let* ((content-types-to-try
                       (etypecase verb
                         (snooze-verbs:sending-verb client-accepted-content-types)
                         (snooze-verbs:receiving-verb
                          (list (or (and content-class-in-uri
                                         *allow-extension-as-accept*)
                                    (parse-content-type-header content-type)
                                    (error 'unsupported-content-type))))))
                     (matching-ct
                       (matching-content-type-or-lose *resource*
                                                      verb
                                                      converted-arguments
                                                      content-types-to-try)))
                (multiple-value-bind (payload code payload-ct)
                    (apply *resource* verb matching-ct converted-arguments)
                  (unless code
                    (setq code (if payload
                                   200 ; OK
                                   204 ; OK, no content
                                   )))
                  (cond (payload-ct
                         (when (and (destructive-p verb)
                                    (not (typep payload-ct (class-of matching-ct))))
                           (warn "Route declared ~a as a its payload content-type, but it matched ~a"
                                 payload-ct matching-ct)))
                        (t
                         (setq payload-ct
                               (if (destructive-p verb)
                                   'snooze-types:text/html ; the default
                                   matching-ct))))
                  (throw 'response (values code
                                           payload
                                           (content-class-name payload-ct))))))))))))



(defvar *clack-request-env*)
(defun make-clack-app (&rest args)
  (lambda (env)
    (let ((*clack-request-env* env))
      (multiple-value-bind (status-code payload payload-ct)
          (apply #'handle-request (getf env :request-uri)
                 :method (getf env :request-method)
                 :accept (gethash "accept" (getf env :headers))
                 :content-type (getf env :content-type)
                 args)
        `(,status-code
          (:content-type ,payload-ct)
          (,payload))))))



;;; Internal
;;; 
(defmethod print-object ((c http-condition) s)
  (print-unreadable-object (c s :type t)
    (format s "~a" (status-code c))))







