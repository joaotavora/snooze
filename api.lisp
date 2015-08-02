;;; User facing API.
;;;
;;; Every external symbol here 
;;;
(in-package #:snooze)


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
       ,@(if genurl-form `(,genurl-form))
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
(defparameter *catch-errors* t)

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

(define-condition internal-server-error (http-condition) ()
  (:default-initargs :status-code 500
                     :format-control "Internal server error"))
  
(defgeneric explain-condition (condition content-type)
  (:documentation "Explain CONDITION to client in CONTENT-TYPE.")
  (:method (condition (content-type (eql 'failsafe)))
    (format nil "~a" condition))
  (:method (condition (content-type (eql 'full-backtrace)))
    (format "SNOOZE has this to report:~a~%Backtrace:~%~a~a"
            condition
            (with-output-to-string (s)
              (uiop/image:print-condition-backtrace condition :stream s))))
  (:method (condition (content-type snooze-types:text/html))
    (format nil "<p>You seem to have triggered a <i>~a</i></p>" (status-code condition))))


;;; Advanced
;;;
(defgeneric convert-arguments (resource plain-arguments keyword-arguments)
  (:method (resource plain-arguments keyword-arguments)
    (flet ((probe (value)
             (or (let ((*read-eval* nil))
                   (ignore-errors
                    (read-from-string value)))
                 value)))
    (append
     (mapcar #'probe plain-arguments)
     (loop for (key value) in keyword-arguments by #'cddr
           collect key
           collect (probe value)))))
  (:documentation
   "In the context of SERVER, make ACTUAL-ARGUMENTS fit RESOURCE.
Should return a list of the same length as ACTUAL-ARGUMENTS, which is
a list of strings, but where some strings have been converted to other
types.  The default method tries to convert every arguments to a
number."))


;;;
;;;
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
      (error 'snooze:no-such-route
             :status-code (if (typep snooze-verbs:sending-verb verb)
                              )

             )))

(defun handle-request (uri
                       &key
                         (method :get)
                         (accept "*/*")
                         (content-type "application/x-www-form-urlencoded")
                         (resources
                          *all-resources* resources-provided-p)
                         (home-resource
                          nil home-resource-provided-p)
                         (resource-name-regexp
                          "/([^/.]+)" resource-name-regexp-provided-p)
                         (catch-http-conditions *catch-http-conditions*)
                         (catch-errors *catch-errors*)
                         (allow-extension-as-accept t)
                         (respect-accept-on-conditions nil))
  (block retval-block
    (multiple-value-bind (resource pargs kwargs content-class-in-uri)
        (apply #'parse-resource (puri:parse-uri uri)
               `(,@(if resource-name-regexp-provided-p
                       `(:resource-name-regexp ,resource-name-regexp))
                 ,@(if resources-provided-p
                       `(:resources ,resources))
                 ,@(if home-resource-provided-p
                       `(:home-resource ,home-resource))))
      (let* ((verb (find-verb-or-lose method))
             (client-accepted-content-types
               `(,@(if (and content-class-in-uri
                            allow-extension-as-accept)
                       (list content-class-in-uri))
                 ,@(or (content-classes-in-accept-string accept)
                       (list (find-content-class 'snooze-types:text/plain))))))
        (flet ((respond (code body content-type-designator)
                 (return-from retval-block
                   (values code body (find-content-class content-type-designator)))))
          (handler-bind ((error
                           (lambda (e)
                             (cond ((eq catch-errors :backtrace)
                                    (respond 500
                                            (explain-condition e 'full-backtrace)
                                            'text/plain))
                                   (catch-errors
                                    (respond 500
                                             (explain-condition e 'failsafe)
                                             'text/plain)))))
                         (http-condition
                           (lambda (c)
                             (cond ((eq catch-http-conditions :backtrace)
                                    (respond
                                     (status-code c)
                                     (explain-condition c 'full-backtrace)
                                     'text/plain))))))
            (flet ((explain-condition-to-client (code condition)
                     (let ((response-content-type
                             (some (lambda (wanted)
                                     (when (gf-primary-method-specializer
                                            #'explain-condition
                                            (list condition wanted)
                                            1)
                                       wanted))
                                   (mapcar #'make-instance
                                           (append client-accepted-content-types
                                                   (unless respect-accept-on-conditions
                                                     '(snooze-types:text/html)))))))
                       (respond code
                                (explain-condition condition response-content-type)
                                response-content-type))))
              (handler-bind ((http-condition
                               (lambda (c)
                                 (when (and catch-http-conditions
                                            (not (eq catch-http-conditions :backtrace)))
                                   (explain-condition-to-client (status-code c) c))))
                             (error
                               (lambda (e)
                                 (when (and catch-errors
                                            (not (eq catch-errors :backtrace)))
                                   (explain-condition-to-client 501 e)))))
                (unless resource
                  (error 'snooze:no-such-resource
                         :resource nil
                         :verb verb
                         :format-control
                         "So sorry, but that URI doesn't match any REST resources"))
                (let ((converted-arguments (convert-arguments resource pargs kwargs)))
                  (cond ((not (arglist-compatible-p resource converted-arguments))
                         (error 'snooze:invalid-resource-arguments
                                :format-control
                                "Too many, too few, or unsupported query arguments for REST resource ~a"
                                :format-arguments
                                (list resource)))
                        (t
                         (let* ((content-types-to-try
                                  (etypecase verb
                                    (snooze-verbs:sending-verb client-accepted-content-types)
                                    (snooze-verbs:receiving-verb
                                     (list (or (and content-class-in-uri
                                                    allow-extension-as-accept)
                                               (parse-content-type-header content-type)
                                               (error "unknown content type"))))))
                                (matching-ct
                                  (matching-content-type-or-lose resource
                                                                 verb
                                                                 converted-arguments
                                                                 content-types-to-try)))
                           (multiple-value-bind (payload code payload-ct headers)
                               (apply resource
                                      verb
                                      matching-ct
                                      converted-arguments)
                             (unless code
                               (setq code (nominal-code-for-verb verb)))
                             (cond (payload-ct
                                    (when (and (typep verb snooze-verbs:sending-verb)
                                               (not (typep payload-ct (class-of matching-ct))))
                                      (warn "Route declared ~a as a its payload content-type, but it matched ~a"
                                            payload-ct matching-ct)))
                                   (t
                                    (setq payload-ct
                                          (if (typep verb snooze-verbs:sending-verb)
                                              matching-ct
                                              ;; default
                                              'snooze-types:text/html))))
                             (setq headers
                                   (remove-duplicates
                                    `((:content-type . ,payload-ct)
                                      ,@(nominal-headers-for-verb verb uri)
                                      ,@headers)
                                    :key #'car)))
                           

                           
                           (respond 200
                                    (apply resource
                                           verb
                                           matching
                                           converted-arguments)
                                    matching)))))))))))))



;;; Internal
;;; 


(defmethod print-object ((c http-condition) s)
  (format s "HTTP ~a: ~?" (status-code c)
            (simple-condition-format-control c)
            (simple-condition-format-arguments c)))




