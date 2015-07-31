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
(defparameter *debug-on-conditions-p* nil)

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


;;; Advanced
;;;
(defgeneric convert-arguments (resource actual-arguments)
  (:method (resource actual-arguments)
    (loop for (key value) in actual-arguments by #'cddr
          for probe = (let ((*read-eval* nil))
                        (ignore-errors
                         (read-from-string value)))
          collect key
          collect (or probe value)))
  (:documentation
   "In the context of SERVER, make ACTUAL-ARGUMENTS fit RESOURCE.
Should return a list of the same length as ACTUAL-ARGUMENTS, which is
a list of strings, but where some strings have been converted to other
types.  The default method tries to convert every arguments to a
number."))



;;; Internal
;;; 


(defmethod print-object ((c http-condition) s)
  (format s "HTTP ~a: ~?" (status-code c)
            (simple-condition-format-control c)
            (simple-condition-format-arguments c)))




