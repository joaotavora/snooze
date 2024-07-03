(in-package #:snooze-common)


;;; Verbs
;;;
;;; "Sending" and "Receiving" are always from the server's
;;; perspective. Hence GET is "sending to client" and POST and PUT are
;;; "receiving from client".
;;;
(defpackage :snooze-verbs (:use)
            (:export #:http-verb #:get #:post #:patch #:put #:delete
                     #:content-verb
                     #:receiving-verb
                     #:sending-verb))

(cl:defclass snooze-verbs:http-verb      () ())
(cl:defclass snooze-verbs:delete         (snooze-verbs:http-verb) ())
(cl:defclass snooze-verbs:content-verb   (snooze-verbs:http-verb) ())
(cl:defclass snooze-verbs:receiving-verb (snooze-verbs:content-verb) ())
(cl:defclass snooze-verbs:sending-verb   (snooze-verbs:content-verb) ())
(cl:defclass snooze-verbs:post           (snooze-verbs:receiving-verb) ())
(cl:defclass snooze-verbs:patch          (snooze-verbs:receiving-verb) ())
(cl:defclass snooze-verbs:put            (snooze-verbs:receiving-verb) ())
(cl:defclass snooze-verbs:get            (snooze-verbs:sending-verb) ())

(defun destructive-p (verb) (or (typep verb 'snooze-verbs:receiving-verb)
                                (typep verb 'snooze-verbs:delete)))


;;; Content-types
;;;
;;; For PUT and POST requests we match routes based on what the client
;;; declares to us in its "Content-Type" header. At most one CLOS
;;; primary method may match.
;;;
;;; In GET requests we are only interested in the request's "Accept"
;;; header, since GET never have useful bodies (1) and as such don't
;;; have "Content-Type". For GET requests, the logic is actually
;;; inverse: the routes are matched based on what the client accepts.
;;; If it accepts a range of content-types, multiple routes (or
;;; primary CLOS methods) are now eligible. We try many routes in
;;; order (according to that range) until we find one that matches.
;;;
;;; [1]: http://stackoverflow.com/questions/978061/http-get-with-request-body
;;;
(defclass snooze-types:content () ())

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun intern-safe (designator package)
    (intern (string-upcase designator) package))
  (defun scan-to-strings* (regex string)
    (coerce (nth-value 1
                       (cl-ppcre:scan-to-strings regex
                                                 string))
            'list)))

(defmacro define-content (type-designator
                          &optional (supertype-designator
                                     (first (scan-to-strings*
                                             "([^/]+)" type-designator))))
  (let* ((type (intern-safe type-designator :snooze-types))
         (supertype (intern-safe supertype-designator :snooze-types)))
    `(progn
       (setf (get ',type 'name) ,(string-downcase (symbol-name type)))
       (unless (find-class ',supertype nil)
         (setf (get ',supertype 'name)
               ,(format nil "~a/*"
                        (string-downcase (symbol-name supertype))))
         (defclass ,supertype (snooze-types:content) ()))
       (defclass ,type (,supertype) ())
       (eval-when (:compile-toplevel :load-toplevel :execute)
         (export '(,type ,supertype) :snooze-types)))))

(defmacro define-known-content-types ()
  `(progn
     ,@(loop for (type-spec . nil) in *mime-type-list*
             for matches
               = (nth-value
                  1 (cl-ppcre:scan-to-strings "(.*/.*)(?:;.*)?" type-spec))
             for type = (and matches (aref matches 0))
             when type
               collect `(define-content ,type))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-known-content-types))

(defun find-content-class (designator)
  "Return class for DESIGNATOR if it defines a content-type or nil."
  (cond ((typep designator 'snooze-types:content)
         (class-of designator))
        ((and (typep designator 'class)
              (subtypep designator 'snooze-types:content))
         designator)
        ((eq designator t)
         (alexandria:simple-style-warning
          "Coercing content-designating type designator T to ~s"
          'snooze-types:content)
         (find-class 'snooze-types:content))
        ((or (symbolp designator)
             (stringp designator))
         (or (find-class (intern (string-upcase designator) :snooze-types) nil)
             (and (string= designator "*/*") (find-class 'snooze-types:content))
             (let* ((matches (nth-value 1
                                        (cl-ppcre:scan-to-strings
                                         "([^/]+)/\\*"
                                         (string-upcase designator))))
                    (supertype-designator (and matches
                                               (aref matches 0))))
               (find-class
                (intern (string-upcase supertype-designator) :snooze-types)
                nil))))
        (t
         (error "~a cannot possibly designate a content-type" designator))))

(defun content-class-name (designator)
  (get (class-name (find-content-class designator)) 'name))



;;; Resources
;;;
(defun resource-p (thing)
  (and (functionp thing)
       (eq 'resource-generic-function (type-of thing))))

(deftype resource ()
  `(satisfies resource-p))

(defclass resource-generic-function (cl:standard-generic-function)
  ()
  (:metaclass closer-mop:funcallable-standard-class))

(defun resource-name (resource)
  (closer-mop:generic-function-name resource))

(defvar *all-resources* (make-hash-table))

(defun find-resource (designator &key filter)
  (cond ((or (stringp designator)
             (keywordp designator))
         (maphash (lambda (k v)
                    (when (and (string-equal (string k) (string designator))
                               (or (not filter)
                                   (funcall filter v)))
                      (return-from find-resource v)))
                  *all-resources*))
        ((resource-p designator)
         (find-resource (resource-name designator)
                        :filter filter))
        ((and designator
              (symbolp designator))
         (let ((probe (gethash designator *all-resources*)))
           (when (or (not filter)
                     (funcall filter designator))
             probe)))
        (t
         (error "~a ins't a resource designator" designator))))

(defun delete-resource (designator)
  (let ((resource (find-resource designator)))
    (cond (resource
           (fmakunbound (resource-name resource))
           (remhash (resource-name resource) *all-resources*))
          (t
           (error "No such resource to delete!")))))

(defmethod initialize-instance :after
    ((gf resource-generic-function) &rest args)
  (declare (ignore args))
  (setf (gethash (resource-name gf) *all-resources*)
        gf))

(defun probe-class-sym (sym)
  "Like CL:FIND-CLASS but don't error and return SYM or nil"
  (when (find-class sym nil)
    sym))

(defun parse-defroute-args (defmethod-arglist)
  "Return values QUALIFIERS, LAMBDA-LIST, BODY for DEFMETHOD-ARGLIST"
  (loop for args on defmethod-arglist
        if (listp (first args))
          return (values qualifiers (first args) (cdr args))
        else
          collect (first args) into qualifiers))

(defun verb-spec-or-lose (verb-spec)
  "Convert VERB-SPEC into something CL:DEFMETHOD can grok."
  (labels ((verb-designator-to-verb (designator)
             (or (and (eq designator 't)
                      (progn
                        (alexandria:simple-style-warning
                         "Coercing verb-designating type T in ~a to ~s"
                         verb-spec 'snooze-verbs:http-verb)
                        'snooze-verbs:http-verb))
                 (probe-class-sym (intern (string-upcase designator)
                                          :snooze-verbs))
                 (error "Sorry, don't know the HTTP verb ~a"
                        (string-upcase designator)))))
    (cond ((and verb-spec
                (listp verb-spec))
           (list (first verb-spec)
                 (verb-designator-to-verb (second verb-spec))))
          ((or (keywordp verb-spec)
               (stringp verb-spec))
           (list 'snooze-verbs:http-verb (verb-designator-to-verb verb-spec)))
          (verb-spec
           (list verb-spec 'snooze-verbs:http-verb))
          (t
           (error "~a is not a valid convertable HTTP verb spec" verb-spec)))))



(defun content-type-spec-or-lose-1 (type-spec)
  (labels ((type-designator-to-type (designator)
             (let ((class (find-content-class designator)))
               (if class (class-name class)
                   (error "Sorry, don't know the content-type ~a" type-spec)))))
    (cond ((and type-spec
                (listp type-spec))
           (list (first type-spec)
                 (type-designator-to-type (second type-spec))))
          ((or (keywordp type-spec)
               (stringp type-spec))
           (list 'snooze-types:type (type-designator-to-type type-spec)))
          (type-spec
           (list type-spec (type-designator-to-type t))))))

(defun content-type-spec-or-lose (type-spec verb)
  (cond ((subtypep verb 'snooze-verbs:content-verb)
         (content-type-spec-or-lose-1 type-spec))
        ((and type-spec (listp type-spec))
         ;; specializations are not allowed on DELETE, for example
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

(defun ensure-uri (maybe-uri)
  (etypecase maybe-uri
    (string (quri:uri maybe-uri))
    (quri:uri maybe-uri)))

(defun parse-resource (uri)
  "Parse URI for a resource and how it should be called.

Honours of *RESOURCE-NAME-FUNCTION*, *RESOURCES-FUNCTION*,
*HOME-RESOURCE* and *URI-CONTENT-TYPES-FUNCTION*.

Returns nil if the resource cannot be found, otherwise returns 3
values: RESOURCE, URI-CONTENT-TYPES and RELATIVE-URI. RESOURCE is a
generic function verifying RESOURCE-P discovered in URI.
URI-CONTENT-TYPES is a list of subclasses of SNOOZE-TYPES:CONTENT
discovered in directly URI by
*URI-CONTENT-TYPES-FUNCTION*. RELATIVE-URI is the remaining URI after
these discoveries."
  ;; <scheme name> : <hierarchical part> [ ? <query> ] [ # <fragment> ]
  ;;
  (let ((uri (ensure-uri uri))
        uri-stripped-of-content-type-info
        uri-content-types)
    (when *uri-content-types-function*
      (multiple-value-setq (uri-content-types uri-stripped-of-content-type-info)
        (funcall *uri-content-types-function*
                 (quri:render-uri uri nil))))
    (let* ((uri (ensure-uri (or uri-stripped-of-content-type-info
                                uri))))
      (multiple-value-bind (resource-name relative-uri)
          (funcall *resource-name-function*
                   (quri:render-uri uri))
        (setq resource-name (and resource-name
                                 (plusp (length resource-name))
                                 (ignore-errors
                                  (quri:url-decode resource-name))))
        (values (find-resource (or resource-name
                                   *home-resource*)
                               :filter *resource-filter*)
                (mapcar #'find-content-class uri-content-types)
                relative-uri)))))

(defun content-classes-in-accept-string (string)
  (labels ((expand (class)
             (cons class
                   (reduce
                    #'append
                    (mapcar #'expand
                            (closer-mop:class-direct-subclasses class))))))
    (loop for media-range-and-params in (cl-ppcre:split "\\s*,\\s*" string)
          for class = (parse-content-type-header media-range-and-params)
          when class
            append (expand class))))

(defun parse-content-type-header (string)
  "Return a class associated with the content-type described by STRING.
As a second value, return what RFC2388:PARSE-HEADER"
  (let* ((parsed (rfc2388:parse-header string :value))
         (designator (second parsed)))
    (values (find-content-class designator)
            parsed)))

(defun find-verb-or-lose (designator)
  (let ((class (or (probe-class-sym
                    (intern (string-upcase designator)
                            :snooze-verbs))
                   (error "Can't find HTTP verb for designator ~a!"
                          designator))))
    ;; FIXME: perhaps use singletons here
    (make-instance class)))

(defun gf-primary-method-specializer (gf args ct-arg-pos)
  "Compute proper content-type for calling GF with ARGS"
  (let ((applicable (compute-applicable-methods gf args)))
    (when applicable
      (nth ct-arg-pos (closer-mop:method-specializers (first applicable))))))



;;; Internal symbols of :SNOOZE
;;;
(in-package :snooze)

(defun check-arglist-compatible (resource args)
  (let ((lambda-list (closer-mop:generic-function-lambda-list
                      resource)))
    (handler-case
        ;; FIXME: evaluate this need for eval, for security reasons
        (let ((*read-eval* nil))
          (handler-bind ((warning #'muffle-warning))
            (eval `(apply (lambda ,lambda-list
                            t)
                          '(t t ,@args)))))
      (error (e)
        (error 'incompatible-lambda-list
               :actual-args args
               :lambda-list (cddr lambda-list)
               :format-control "Too many, too few, or unsupported ~
                                query arguments for REST resource ~a"
               :format-arguments
               (list (resource-name resource))
               :original-condition e)))))

(defun check-optional-args (opt-values &optional warn-p)
  (let ((nil-tail
          (member nil opt-values)))
    (unless (every #'null (rest nil-tail))
      (if warn-p
          (warn 'style-warning :format-control
                "The NIL defaults to a genpath-function's &OPTIONALs ~
               must be at the end")
          (error "The NILs to a genpath-function's &OPTIONALs ~
                must be at the end")))))

(defun genpath-fn-lambda-list (all-kwargs
                               augmented-optional
                               required
                               rest
                               aok-p)
  "Helper for MAKE-GENPATH-FORM"
  `(,@required
    &optional
    ,@augmented-optional
    ,@(if rest
          (warn 'style-warning
                :format-control
                "&REST ~a is not supported for genpath-functions"
                :format-arguments (list rest)))
    &key
    ,@all-kwargs
    ,@(if aok-p `(&allow-other-keys))))

(defun make-genpath-form (genpath-fn-name resource-sym lambda-list)
  (multiple-value-bind (required optional rest kwargs aok-p aux key-p)
      (alexandria:parse-ordinary-lambda-list lambda-list)
    (declare (ignore aux key-p))
    (let* (;;
           ;;
           (augmented-optional
             (loop for (name default nil) in optional
                   collect `(,name ,default ,(gensym))))
           ;;
           ;;
           (augmented-kwargs
             (loop for (kw-and-sym default) in kwargs
                   collect `(,kw-and-sym ,default ,(gensym))))
           ;;
           ;;
           (all-kwargs
             augmented-kwargs)
           ;;
           ;;
           (required-args-form
             `(list ,@required))
           ;;
           ;;
           (optional-args-form
             `(list ,@(loop for (name default supplied-p) in augmented-optional
                            collect `(if ,supplied-p ,name
                                         (or ,name ,default)))))
           ;;
           ;;
           (keyword-arguments-form
             `(remove-if #'null
                         (list
                          ,@(loop for (kw-and-sym default supplied-p)
                                    in augmented-kwargs
                                  for (nil sym) = kw-and-sym
                                  collect `(cons (intern
                                                  (symbol-name ',sym)
                                                  (find-package :KEYWORD))
                                                 (if ,supplied-p
                                                     ,sym
                                                     (or ,sym
                                                         ,default)))))
                         :key #'cdr)))
      ;; Optional args are checked at macroexpansion time
      ;;
      (check-optional-args (mapcar #'second optional) 'warn-p)
      `(progn
         (defun ,genpath-fn-name
             ,(genpath-fn-lambda-list
               all-kwargs
               augmented-optional
               required
               rest
               aok-p)
           ;; And at runtime...
           ;;
           (check-optional-args ,optional-args-form)
           (arguments-to-uri
            (find-resource ',resource-sym)
            (append
             ,required-args-form
             (remove nil ,optional-args-form))
            ,keyword-arguments-form)
           )))))

(defun defroute-1 (name args)
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
         (type-spec (content-type-spec-or-lose (second lambda-list)
                                               (second verb-spec)))
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

(defun defgenpath-1 (function resource)
  (make-genpath-form function resource
                     (nthcdr 2 (closer-mop:generic-function-lambda-list
                                (let ((probe (find-resource resource)))
                                  (assert probe nil
                                          "Cannot find the resource ~a"
                                          resource)
                                  probe)))))

(defun defresource-1 (name lambda-list options)
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
                                      (content-type-spec-or-lose
                                       (second spec-list)
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



;;; Some external stuff but hidden away from the main file
;;;
(defmethod explain-condition-failsafe (condition resource &optional verbose-p)
  (declare (ignore resource))
  (let* ((original-condition (and (typep condition 'resignalled-condition)
                                  (original-condition condition)))
         (status-code (or (and original-condition
                               (typep original-condition 'http-condition)
                               (status-code original-condition))
                          500)))
    (with-output-to-string (s)
      (cond (verbose-p
             (format s "~a" condition)
             (explain-failsafe condition s)
             (loop for (condition backtrace) in *useful-backtraces*
                   do (format s "~&~%Here's a backtrace for condition ~s~
                   ~&~a" condition backtrace)))
            (t
             (format s "~a ~a"
                     status-code
                     (reason-for status-code)))))))

(define-condition http-condition (simple-condition)
  ((status-code :initarg :status-code
                :initform (error "Must supply a HTTP status code.")
                :reader status-code))
  (:default-initargs :format-control "HTTP condition"))

(define-condition http-error (http-condition simple-error) ()
  (:default-initargs
   :format-control "HTTP Internal Server Error"
   :status-code 500))

(define-condition no-such-resource (http-condition) ()
  (:default-initargs
   :status-code 404
   :format-control "Resource does not exist"))

(define-condition invalid-resource-arguments (http-condition) ()
  (:default-initargs
   :status-code 400
   :format-control "Resource exists but invalid arguments passed"))

(define-condition resignalled-condition ()
  ((original-condition :initarg :original-condition
                       :initform (error "Must supply an original condition")
                       :reader original-condition)))

(define-condition unconvertible-argument
    (invalid-resource-arguments resignalled-condition)
  ((unconvertible-argument-value :initarg :unconvertible-argument-value
                                 :accessor unconvertible-argument-value)
   (unconvertible-argument-key :initarg :unconvertible-argument-key
                               :accessor unconvertible-argument-key))
  (:default-initargs
   :format-control "An argument in the URI cannot be read"))

(define-condition incompatible-lambda-list
    (invalid-resource-arguments resignalled-condition)
  ((lambda-list :initarg :lambda-list
                :initform (error "Must supply :LAMBDA-LIST")
                :accessor lambda-list)
   (actual-args :initarg :actual-args
                :initform (error "Must supply :ACTUAL-ARGS")
                :accessor actual-args))
  (:default-initargs
   :format-control "An argument in the URI cannot be read"))

(define-condition invalid-uri-structure
    (invalid-resource-arguments resignalled-condition)
  ((invalid-uri :initarg :invalid-uri
                :initform (error "Must supply the invalid URI")
                :accessor invalid-uri))
  (:default-initargs
   :format-control "The URI structure cannot be converted into arguments"))

(define-condition  unsupported-content-type (http-error) ()
  (:default-initargs
   :status-code 501
   :format-control "Content type is not supported"))

(define-condition no-such-route (http-condition) ()
  (:default-initargs
   :format-control "Resource exists but no such route"))

(define-condition error-when-explaining (simple-error resignalled-condition) ()
  (:default-initargs
   :format-control "An error occurred when trying to explain a condition"))

(defmethod print-object ((c http-condition) s)
  (print-unreadable-object (c s :type t)
    (format s "~a: ~?" (status-code c)
            (simple-condition-format-control c)
            (simple-condition-format-arguments c))))

(defmethod print-object ((c resignalled-condition) s)
  (print-unreadable-object (c s :type t)
    (princ (original-condition c) s)))

(defmethod explain-failsafe ((c condition) s)
  ;; (format s "~&~%No more interesting information on ~a, sorry~%" c)
  )

(defmethod explain-failsafe ((c error-when-explaining) s)
  (format s "~&  SNOOZE:EXPLAIN-CONDITION is missing a method to politely explain:~
             ~&    ~a~
             ~&  to the client."
          (original-condition c)))

(defmethod explain-failsafe ((c unconvertible-argument) s)
  (format s "~&  SNOOZE:URI-TO-ARGUMENTS caught a ~a when converting:~
             ~&    ~a=~a~
             ~&  into Lisp objects to give to your route."
          (type-of (original-condition c))
          (unconvertible-argument-key c)
          (unconvertible-argument-value c)))

(defmethod explain-failsafe ((c invalid-uri-structure) s)
  (format s "~&  SNOOZE:URI-TO-ARGUMENTS can't grok this URI:~
             ~&    ~a" (invalid-uri c)))

(defmethod explain-failsafe ((c incompatible-lambda-list) s)
  (format s "~&  Snooze failed to fit:~
             ~&    ~s~
             ~&  to the lambda list:~
             ~&    ~a~
             ~&  which produced a ~a which your Lisp describes as:~
             ~&    ~a"
          (actual-args c) (lambda-list c)
          (type-of (original-condition c))
          (original-condition c)))

(defmethod explain-failsafe :before ((c resignalled-condition) s)
  (format s "~&~%You got a ~a because:~% " (type-of c)))

(defmethod explain-failsafe :after ((c resignalled-condition) s)
  (explain-failsafe (original-condition c) s))


;;; More internal stuff
;;;

(defmethod initialize-instance :after ((e http-error) &key)
  (assert (<= 500 (status-code e) 599) nil
          "An HTTP error must have a status code between 500 and 599"))

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
             :status-code (if try-list
                              (if (destructive-p verb)
                                  415 ; unsupported media type
                                  406 ; not acceptable
                                  )
                              ;; FIXME, make "unimplemented" more pervasive
                              501 ; unimplemented
                              ))))

(defvar *useful-backtraces* nil "Useful backtraces.")

(defmacro saving-useful-backtrace (args &body body)
  (declare (ignore args))
  `(handler-bind
       ((t
          (lambda (e)
            (when *catch-errors*
              (pushnew (list e
                             (with-output-to-string (s)
                               (uiop/image:print-condition-backtrace
                                e :stream s)))
                       *useful-backtraces*
                       :test (lambda (a b) (eq (first a) (first b))))))))
     ,@body))

(defun call-brutally-explaining-conditions (fn)
  (let (code condition original-condition *useful-backtraces*)
    (flet ((explain (verbose-p)
             (throw 'response
               (values code
                       (explain-condition-failsafe condition
                                                   *resource*
                                                   verbose-p)
                       (content-class-name 'text/plain)))))
      (restart-case
          (handler-bind
              ((resignalled-condition
                 (lambda (e)
                   (setq original-condition (original-condition e)
                         code
                         (when (typep original-condition 'http-condition)
                           (status-code original-condition)))))
               (error
                 (lambda (e)
                   (setq code (or code 500)
                         condition e)
                   (cond ((eq *catch-errors* :verbose)
                          (invoke-restart 'explain-verbosely))
                         (*catch-errors*
                          (invoke-restart 'failsafe-explain))
                         (;; HACK! notice that a non-error
                          ;; `http-condition' (like a simple redirect)
                          ;; with `*catch-errors*' = NIL and
                          ;; `*catch-http-conditions*' = T will land
                          ;; us in this branch. We do not want to
                          ;; break in this case, so explain succintly.
                          (and original-condition
                               (typep original-condition 'http-condition)
                               (not (typep original-condition 'error)))
                          (invoke-restart 'failsafe-explain)))))
               (http-condition
                 (lambda (c)
                   (setq code (status-code c) condition c)
                   (cond ((eq *catch-http-conditions* :verbose)
                          (invoke-restart 'explain-verbosely))))))
            (saving-useful-backtrace () (funcall fn)))
        (explain-verbosely () :report
          (lambda (s)
            (format s "Explain ~a condition more verbosely" code))
          (explain t))
        (failsafe-explain () :report
          (lambda (s) (format s "Explain ~a condition very succintly" code))
          (explain nil))))))

(defun call-politely-explaining-conditions (client-accepts fn)
  (let (code
        condition
        accepted-type)
    (labels ((accepted-type-for (condition)
               (some (lambda (wanted)
                       (when (gf-primary-method-specializer
                              #'explain-condition
                              (list condition *resource* wanted)
                              1)
                         wanted))
                     (mapcar #'make-instance client-accepts)))
             (check-politely-explain ()
               (unless accepted-type
                 (error 'error-when-explaining
                        :format-control "No ~a to politely explain ~a to client"
                        :format-arguments
                        (list 'explain-condition (type-of condition))
                        :original-condition condition))))
      (restart-case
          (handler-bind ((condition
                           (lambda (c)
                             (setq
                              condition c
                              accepted-type (accepted-type-for condition))))
                         (http-condition
                           (lambda (c)
                             (setq code (status-code c))
                             (when (and *catch-http-conditions*
                                        (not (eq *catch-http-conditions*
                                                 :verbose)))
                               (check-politely-explain)
                               (invoke-restart 'politely-explain))))
                         (error
                           (lambda (e)
                             (declare (ignore e))
                             (setq code 500)
                             (when (and *catch-errors*
                                        (not (eq *catch-errors* :verbose)))
                               (check-politely-explain)
                               (invoke-restart 'politely-explain)))))
            (saving-useful-backtrace () (funcall fn)))
        (politely-explain ()
          :report (lambda (s)
                    (format s "Politely explain to client in ~a"
                            accepted-type))
          :test (lambda (c) (declare (ignore c)) accepted-type)
          (throw 'response
            (handler-case
                (values code
                        (explain-condition condition *resource* accepted-type)
                        (content-class-name accepted-type))
              (error (e)
                (error 'error-when-explaining
                       :format-control "Error when explaining ~a"
                       :format-arguments (list (type-of e))
                       :original-condition condition)))))
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
              (invoke-restart 'politely-explain)
              (if (find-restart 'failsafe-explain)
                  (invoke-restart 'failsafe-explain))))))))

(defmacro brutally-explaining-conditions (() &body body)
  "Explain conditions in BODY in a failsafe way.
Honours the :VERBOSE option to *CATCH-ERRORS* and *CATCH-HTTP-CONDITIONS*."
  `(call-brutally-explaining-conditions (lambda () ,@body)))

(defmacro politely-explaining-conditions ((client-accepts) &body body)
  "Explain conditions in BODY taking the client accepts into account.
Honours *CATCH-ERRORS* and *CATCH-HTTP-CONDITIONS*"
  `(call-politely-explaining-conditions ,client-accepts (lambda () ,@body)))

(defvar *resource*)
(setf (documentation '*resource* 'variable)
      "Bound early in HANDLE-REQUEST-1 to nil or to a RESOURCE.
Used by POLITELY-EXPLAINING-CONDITIONS and
BRUTALLY-EXPLAINING-CONDITIONS to pass a resource to
EXPLAIN-CONDITION.")

(defun handle-request-1 (uri method accept &optional content-type)
  (catch 'response
    (let (*resource*
          content-classes-encoded-in-uri
          relative-uri)
      (brutally-explaining-conditions ()
        (multiple-value-setq (*resource* content-classes-encoded-in-uri relative-uri)
          (parse-resource uri))
        (let* ((verb (find-verb-or-lose method))
               (client-accepted-content-types
                 (or (append content-classes-encoded-in-uri
                             (content-classes-in-accept-string accept))
                     (list (find-content-class 'snooze-types:text/plain)))))
          (politely-explaining-conditions (client-accepted-content-types)
            (unless *resource*
              (error 'no-such-resource
                     :format-control
                     "So sorry, but that URI doesn't match any REST resources"))
            ;; URL-decode args to strings
            ;;
            (multiple-value-bind (converted-plain-args converted-keyword-args)
                (handler-bind
                    ((error
                       (lambda (e)
                         (when *catch-errors*
                           (error 'invalid-uri-structure
                                  :format-control
                                  "Caught ~a in URI-TO-ARGUMENTS"
                                  :format-arguments (list (type-of e))
                                  :original-condition e
                                  :invalid-uri relative-uri)))))
                  (uri-to-arguments *resource* relative-uri))
              (let ((converted-arguments
                      (append converted-plain-args
                              (loop for (a . b) in converted-keyword-args
                                    collect a collect b))))
                ;; Double check that the arguments indeed
                ;; fit the resource's lambda list
                ;;
                (check-arglist-compatible *resource* converted-arguments)
                (let* ((matching-ct
                         (typecase verb
                           ;; HTTP DELETE doesn't care about
                           ;; content-types
                           (snooze-verbs:delete nil)
                           (t
                            (matching-content-type-or-lose
                             *resource*
                             verb
                             converted-arguments
                             (typecase verb
                               (snooze-verbs:sending-verb
                                client-accepted-content-types)
                               (snooze-verbs:receiving-verb
                                (list (or (and content-classes-encoded-in-uri
                                               (first content-classes-encoded-in-uri))
                                          (parse-content-type-header content-type)
                                          (error 'unsupported-content-type))))))))))
                  (multiple-value-bind (payload code payload-ct)
                      (apply *resource* verb matching-ct converted-arguments)
                    (unless code
                      (setq code (if payload
                                     200 ; OK
                                     204 ; OK, no content
                                     )))
                    (cond (payload-ct
                           (when (and (destructive-p verb)
                                      (not (typep payload-ct
                                                  (class-of matching-ct))))
                             (warn "Route declared ~a as its payload ~
                                    content-type, but it matched ~a"
                                   payload-ct matching-ct)))
                          (t
                           (setq payload-ct
                                 (if (destructive-p verb)
                                     'snooze-types:text/html ; the default
                                     matching-ct))))
                    (throw 'response (values code
                                             payload
                                             (content-class-name
                                              payload-ct)))))))))))))

;;; Default values for options
;;;
(defun default-resource-name (uri)
  "Default value for *RESOURCE-NAME-FUNCTION*, which see."
  (if (string= "" uri)
    ""
    (let* ((first-slash-or-qmark (position-if #'(lambda (char)
                                                  (member char '(#\/ #\?)))
                                              uri
                                              :start 1)))
      (values (cond (first-slash-or-qmark
                     (subseq uri 1 first-slash-or-qmark))
                    (t
                     (subseq uri 1)))
              (if first-slash-or-qmark
                  (subseq uri first-slash-or-qmark))))))

(defun search-for-extension-content-type (uri-path)
  "Default value for *URI-CONTENT-TYPES-FUNCTION*, which see."
  (multiple-value-bind (matchp groups)
      (cl-ppcre:scan-to-strings "([^\\.]+)\\.(\\w+)([^/]*)$" uri-path)
    (let ((content-type-class (and matchp
                                   (find-content-class
                                    (gethash (aref groups 1)
                                             *mime-type-hash*)))))
      (when content-type-class
        (values
         (list content-type-class)
         (format nil "~a~a" (aref groups 0) (aref groups 2)))))))

(defun all-defined-resources ()
  "Default value for *RESOURCES-FUNCTION*, which see."
  snooze-common:*all-resources*)



;;; Reading and writing URI's
;;;
(defun resource-package (resource)
  (symbol-package (resource-name resource)))

(defun uri-to-arguments-1 (resource relative-uri)
  "Do actual work for default method of URI-TO-ARGUMENTS."
  (labels ((probe (str &optional key)
             (handler-bind
                 ((error (lambda (e)
                           (when *catch-errors*
                             (error 'unconvertible-argument
                                    :unconvertible-argument-value str
                                    :unconvertible-argument-key key
                                    :original-condition e
                                    :format-control
                                    "Malformed arg for resource ~a"
                                    :format-arguments
                                    (list (resource-name resource)))))))
               (progn
                 (let ((*read-eval* nil))
                   (read-for-resource resource str)))))
           (probe-keyword (str)
             (let* ((probe (probe str)))
               ;; Though perhaps that keyword is accepted, we may
               ;; still refuse to intern it in the :KEYWORD pacakge
               ;; before trying to use it as a keyword argument, if it
               ;; looks like the symbol didn't "exist" yet.
               ;;
               ;; In other words, we simply require that the symbol
               ;; has a package: it's up to READ-FOR-RESOURCE (the
               ;; default doesn't intern new symbols) to decide if it
               ;; spits out symbols in those conditions.
               ;;
               (if (and (symbolp probe)
                        (symbol-package probe))
                   (intern (symbol-name probe) :keyword)
                   (error 'invalid-resource-arguments
                          :format-control "Unknown keyword for resource ~a"
                          :format-arguments (list (resource-name resource)))))))
    (when relative-uri
      (let* ((relative-uri (ensure-uri relative-uri))
             (path (quri:uri-path relative-uri))
             (query (quri:uri-query relative-uri))
             (fragment (quri:uri-fragment relative-uri))
             (plain-args (and path
                              (plusp (length path))
                              (cl-ppcre:split "/" (subseq path 1))))
             (keyword-args
               (append
                (and
                 query
                 (loop for maybe-pair in (cl-ppcre:split "[;&]" query)
                       for (undecoded-key-name undecoded-value-string)
                         = (scan-to-strings* "(.*)=(.*)" maybe-pair)
                       when (and undecoded-key-name undecoded-value-string)
                         collect
                         (cons (quri:url-decode undecoded-key-name)
                               (quri:url-decode undecoded-value-string)))))))
        (values
         (mapcar #'probe (mapcar #'quri:url-decode plain-args))
         (loop for (key-str . value-str) in keyword-args
               collect (cons (probe-keyword key-str)
                             (probe value-str key-str))
                 into keyword-alist
               finally
                  (return
                    (append
                     keyword-alist
                     (if fragment
                         `((snooze:fragment . ,(probe fragment))))))))))))

(defun arguments-to-uri-1 (resource plain-args keyword-args)
  "Do actual work for default method of ARGUMENTS-TO-URI."
  (flet ((encode (thing &optional keyword)
           (quri:url-encode
            (cond (keyword
                   (string-downcase thing))
                  (t
                   (write-for-resource resource thing)
                   )))))
    (let* ((plain-part (format nil "/~{~a~^/~}"
                               (mapcar #'encode plain-args)))
           (query-part (and keyword-args
                            (format nil "?~{~a=~a~^&~}"
                                    (loop for (k . v) in keyword-args
                                          collect (encode k t)
                                          collect (encode v))))))
      (let ((string (format nil "/~a~a~a"
                            (string-downcase (resource-name resource))
                            plain-part
                            (or query-part ""))))
        string))))

(defun read-for-resource-1 (resource string)
  "Do actual work for default method of READ-FOR-RESOURCE."
  (let ((*package* (resource-package resource)))
    (snooze-safe-simple-read:safe-simple-read-from-string string t)))

(defun write-for-resource-1 (resource object)
  "Do actual work for default-method of WRITE-FOR-RESOURCE."
  (let ((*package* (symbol-package (resource-name resource)))
        (*print-case* :downcase))
    (if (and (symbolp object)
             (not (symbol-package object)))
        (princ-to-string (string-downcase (symbol-name object)))
        (write-to-string object))))
