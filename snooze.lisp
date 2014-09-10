(in-package #:snooze)


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
     If NIL, issue a 404. Otherwise let the ancestors of the
     REST-ACCEPTOR object handle the request")
   (home-resource
    :initform nil :initarg :home-resource
    :accessor home-resource
    :documentation
    "Default \"home\" resource, served when the requested URL is \"bare\".
     Value can be a string or a function designator."))
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
(defpackage :snooze-verbs (:use) (:export #:http-verb #:get #:post #:put #:delete
                                           #:content-verb
                                           #:receiving-verb
                                           #:sending-verb))
(in-package :snooze-verbs)

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
(cl:in-package :snooze)
;; (delete-package :snooze-types)
(defpackage :snooze-types (:use) (:export #:content))

(eval-when (:compile-toplevel :load-toplevel :execute)

  (defclass supertype-metaclass (standard-class) ())

  (defmethod closer-mop:validate-superclass ((class supertype-metaclass)
                                             (superclass standard-class))
    t)

  (defmethod closer-mop:validate-superclass ((superclass standard-class)
                                             (class supertype-metaclass))
    t)

  (defclass snooze-types:content ()
    ((content-body :initarg :content-body
                   :accessor content-body
                   :documentation "A sequence containing the body of the
                     request that the route decided to handle."))
    (:metaclass supertype-metaclass))
  
  (defun intern-safe (designator package)
    (intern (string-upcase designator) package))
  (defun send-any-symbol (supertype)
    (intern (string-upcase (format nil "SEND-ANY-~a" supertype))
            :snooze-types))
  (defun scan-to-strings* (regex string)
    (coerce (nth-value 1
                       (cl-ppcre:scan-to-strings regex
                                                 string))
            'list)))



(defmacro define-content (type-designator
                          &optional (supertype-designator
                                     (first (scan-to-strings* "([^/]+)" type-designator))))
  (let* ((type (intern-safe type-designator :snooze-types))
         (supertype (intern-safe supertype-designator :snooze-types)))
    `(progn
       (unless (find-class ',supertype nil)
         (defclass ,supertype (snooze-types:content) ()
           (:metaclass supertype-metaclass) ))
       (defclass ,type (,supertype) ())
       (eval-when (:compile-toplevel :load-toplevel :execute)
         (export '(,type ,supertype) :snooze-types)))))

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

(defun parse-defroute-args (defmethod-arglist)
  "Return values QUALIFIERS, LAMBDA-LIST, BODY for DEFMETHOD-ARGLIST"
  (loop for args on defmethod-arglist
        if (listp (first args))
          return (values qualifiers (first args) (cdr args))
        else
          collect (first args) into qualifiers))

(defun check-optional-args (opt-values &optional warn-p)
  (let ((nil-tail
          (member nil opt-values)))
  (unless (every #'null (rest nil-tail))
    (if warn-p
        (warn 'style-warning :format-control
              "The NIL defaults to a genurl-function's &OPTIONALs must be at the end")
        (error "The NILs to a genurl-function's &OPTIONALs must be at the end")))))

(defpackage :snooze-syms
  (:use)
  (:export #:protocol #:host
           #:protocol-supplied-p #:host-supplied-p))

(defun make-genurl-form (genurl-fn-name resource-sym lambda-list)
  (multiple-value-bind (required optional rest kwargs aok-p aux key-p)
      (alexandria:parse-ordinary-lambda-list lambda-list)
    (declare (ignore aux key-p))
    (let* (;;
           ;;
           (augmented-optional
             (loop for (name default nil) in optional
                   collect `(,name ,default ,(intern
                                              (format nil "CALLER-SUPPLIED-~A"
                                                      (string-upcase name))
                                              :snooze-syms))))
           ;;
           ;;
           (augmented-kwargs
             (loop for (kw-and-sym default) in kwargs
                   for (nil sym) = kw-and-sym
                   collect `(,kw-and-sym ,default ,(intern
                                                    (format nil "CALLER-SUPPLIED-~A"
                                                            (string-upcase sym))
                                                    :snooze-syms))))
           ;;
           ;;
           (protocol-kwarg-name-sym (if (find :protocol kwargs
                                              :key #'caar)
                                        'snooze-syms:protocol :protocol))
           (host-kwarg-name-sym (if (find :host kwargs
                                          :key #'caar)
                                    'snooze-syms:host :host))
           (host-sym (gensym))
           (protocol-sym (gensym))
           ;;
           ;;
           (all-kwargs
             (append augmented-kwargs
                     `(((,protocol-kwarg-name-sym ,protocol-sym) :http
                        snooze-syms:protocol-supplied-p)
                       ((,host-kwarg-name-sym ,host-sym) nil
                        snooze-syms:host-supplied-p))))
           ;;
           ;;
           (required-args-form
             `(list ,@required))
           ;;
           ;;
           (optional-args-form
             `(list ,@(loop for (name default supplied-p) in augmented-optional
                            collect `(if ,supplied-p ,name (or ,name ,default)))))
           ;;
           ;;
           (keyword-arguments-form
             `(alexandria:flatten
               (remove-if #'null
                          (list
                           ,@(loop for (kw-and-sym default supplied-p)
                                     in augmented-kwargs
                                   for (nil sym) = kw-and-sym
                                   collect `(list (string-downcase ',sym)
                                                  (if ,supplied-p
                                                      ,sym
                                                      (or ,sym
                                                          ,default)))))
                          :key #'second))))
      ;; Optional args are checked at macroexpansion time
      ;;
      (check-optional-args (mapcar #'second optional) 'warn-p)
      `(defun ,genurl-fn-name
           ,@`(;; Nasty, this could easily be a function.
               ;; 
               (,@required
                &optional
                  ,@augmented-optional
                  ,@(if rest
                        (warn 'style-warning
                              :format-control "&REST ~a is not supported for genurl-functions"
                              :format-arguments (list rest)))
                &key
                  ,@all-kwargs
                  ,@(if aok-p `(&allow-other-keys)))
               ;; And at runtime...
               ;;
               (check-optional-args ,optional-args-form)
               (if (and snooze-syms:protocol-supplied-p
                        (not snooze-syms:host-supplied-p))
                   (error "It makes no sense to pass non-NIL ~%  ~a~%and a NIL~%  ~a"
                          (list ',protocol-kwarg-name-sym ,protocol-sym)
                          (list ',host-kwarg-name-sym ,host-sym)))
               (let* ((base-part (and ,host-sym
                                      (format nil "~a://~a/"
                                              (string-downcase
                                               ,protocol-sym)
                                              ,host-sym)))
                      (required-args-list ,required-args-form)
                      (required-part (format nil "~{~a~^/~}" required-args-list))
                      (optional-args-list (remove nil ,optional-args-form))
                      (optional-part (and optional-args-list
                                          (format nil "/~{~a~^/~}" optional-args-list)))
                      (flattened-keywords-list ,keyword-arguments-form)
                      (query-part (and flattened-keywords-list
                                       (format nil "?~{~a=~a~^&~}" flattened-keywords-list))))
                 (format nil "~a~a~a~a~a"
                         (or base-part "")
                         (string-downcase ',resource-sym)
                         (or required-part "")
                         (or optional-part "")
                         (or query-part ""))))))))

(defun verb-spec-or-lose (verb-spec)
  "Convert VERB-SPEC into something `defmethod' can grok."
  (labels ((verb-designator-to-verb (designator)
             (or (and (eq designator 't)
                      (progn
                        (alexandria:simple-style-warning
                         "Coercing verb-designating type T in ~a to ~s"
                              verb-spec 'snooze-verbs:http-verb)
                        'snooze-verbs:http-verb))
                 (find-class-1 (intern (string-upcase designator)
                                       :snooze-verbs))
                 (error "Sorry, don't know the HTTP verb ~a"
                        (string-upcase designator)))))
    (cond ((and verb-spec
                (listp verb-spec))
           (list (first verb-spec) (verb-designator-to-verb (second verb-spec))))
          ((or (keywordp verb-spec)
               (stringp verb-spec))
           (list 'verb (verb-designator-to-verb verb-spec)))
          (verb-spec
           (list verb-spec 'snooze-verbs:http-verb))
          (t
           (error "~a is not a valid convertable HTTP verb spec" verb-spec)))))

(defun find-content-class (designator)
  "Return class for DESIGNATOR if it defines a content-type or nil."
  (or (and (eq designator t)
           (progn
             (alexandria:simple-style-warning
              "Coercing content-designating type designator T to ~s"
              'snooze-types:content)
             (find-class 'snooze-types:content)))
      (find-class (intern (string-upcase designator) :snooze-types) nil)
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
  (cond ((subtypep verb 'snooze-verbs:content-verb)
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
                   collect option)))
    `(progn
       (defgeneric ,name ,lambda-list ,@defgeneric-args)
       (defmethod no-applicable-method ((f (eql (function ,name))) &rest args)
         (error 'no-such-route
                :format-control "No applicable route ~%  ~a~%when called with args ~%  ~a" 
                :format-arguments (list f args)))
       (defmethod check-arguments ((f (eql (function ,name))) actual-arguments)
         (apply 
          (lambda ,lambda-list
            (declare (ignore ,@(remove-if #'(lambda (sym)
                                              (eq #\& (aref (symbol-name sym) 0)))
                                          (mapcar #'(lambda (argspec)
                                                      (ensure-atom argspec))
                                                  lambda-list)))))
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


;;; Helpers for our HUNCHENTOOT:ACCEPTOR-DISPATCH-REQUEST method
;;;
(defun parse-args-in-uri (args-string query-string)
  (let* ((query-and-fragment (scan-to-strings* "(?:([^#]+))?(?:#(.*))?$"
                                               query-string))
         (required-args (cl-ppcre:split "/" (subseq args-string (mismatch "/" args-string))))
         (keyword-args (loop for maybe-pair in (cl-ppcre:split "[;&]" (first query-and-fragment))
                             for (key-name value) = (scan-to-strings* "(.*)=(.*)" maybe-pair)
                             when (and key-name value)
                               append (list (intern (string-upcase key-name) :keyword)
                                            value)))
         (fragment (second query-and-fragment)))
    (append required-args
            keyword-args
            (when fragment
              (list 'snooze:fragment fragment)))))

(defun find-resource-by-name (name acceptor)
  (loop for package in (route-packages acceptor)
        for sym = (find-symbol (string-upcase name) package)
          thereis (and (fboundp sym)
                       (symbol-function sym))))

(defun parse-uri (script-name query-string acceptor)
  "Parse URI for ACCEPTOR. Return values RESOURCE ARGS CONTENT-TYPE."
  ;; <scheme name> : <hierarchical part> [ ? <query> ] [ # <fragment> ]
  (let* ((resource-name-regexp (resource-name-regexp acceptor))
         (match (multiple-value-list (cl-ppcre:scan resource-name-regexp
                                                    script-name)))
         (resource-name
           (and (first match)
                (apply #'subseq script-name
                       (if (plusp (length (third match)))
                           (list (aref (third match) 0) (aref (fourth match) 0))
                           (list (first match) (second match))))))
         (first-slash-resource (find-resource-by-name resource-name acceptor))
         (resource (or first-slash-resource
                       (and (home-resource acceptor)
                            (find-resource-by-name (home-resource acceptor)
                                                   acceptor))))
         (script-minus-resource (if first-slash-resource
                                    (subseq script-name (second match))
                                    script-name))
         (extension-match (cl-ppcre:scan "\\.\\w+$" script-minus-resource))
         (args-string (if extension-match
                          (subseq script-minus-resource 0 extension-match)
                          script-minus-resource))
         (extension (if extension-match
                        (subseq script-minus-resource (1+ extension-match))))
         (content-type-class (and extension
                                  (find-content-class
                                   (hunchentoot:mime-type
                                    (format nil "dummy.~a" extension)))))
         (actual-arguments (parse-args-in-uri (if content-type-class
                                                  args-string
                                                  (if (zerop (length args-string))
                                                      ""
                                                      script-minus-resource))
                                              query-string)))
    (values resource
            actual-arguments
            content-type-class)))

(defun parse-accept-header (string acceptor resource)
  "Return a list of class objects designating " 
  (loop for media-range-and-params in (cl-ppcre:split "\\s*,\\s*" string)
        for media-range = (first (scan-to-strings* "([^;]*)"
                                                   media-range-and-params))
        for class = (find-content-class media-range)
        when class
          append (expand-content-type acceptor resource class)))

(defun arglist-compatible-p (resource args)
  (handler-case
      (progn
        (check-arguments resource (append
                                   (list 'dummy 'dummy)
                                   args))
        t)
    (error () nil)))

(defun parse-content-type-header (string)
  "Return a symbol designating a SNOOZE-SEND-TYPE object."
  (find-content-class string))

(defmethod hunchentoot:acceptor-dispatch-request ((acceptor rest-acceptor) request)
  (multiple-value-bind (resource args content-class)
      (parse-uri (hunchentoot:script-name request)
                 (hunchentoot:query-string request)
                 acceptor)
    (let* ((content-class
             (or content-class
                 (parse-content-type-header (hunchentoot:header-in :content-type request))))
           (verb-designator (or (find-class-1
                                 (intern (string-upcase (hunchentoot:request-method request))
                                         :snooze-verbs))
                                (error "Can't find HTTP verb designator for request ~a!" request)))
           ;; FIXME: maybe use singletons here
           (verb (and verb-designator
                      (make-instance verb-designator))) 
           (converted-arguments (convert-arguments acceptor resource args))
           (accepted-classes
             (if content-class (list content-class)
                 (parse-accept-header (hunchentoot:header-in :accept request)
                                      acceptor
                                      resource))))
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
              ((not (arglist-compatible-p resource converted-arguments))
               (error 'no-such-route
                      :format-control
                      "Too many, too few, or unsupported query arguments for REST resource ~a"
                      :format-arguments
                      (list resource)))
              (t
               (etypecase verb
                 ;; For the Accept: header
                 (snooze-verbs:sending-verb
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
                                               converted-arguments))
                                  t))))
                    retval))
                 (snooze-verbs:receiving-verb
                  (apply resource
                         verb
                         (make-instance (class-name content-class) 
                           :content-body
                           (hunchentoot:raw-post-data :request request))
                         converted-arguments)))))))))

(defmethod hunchentoot:acceptor-status-message ((acceptor rest-acceptor) status-code &key
                                                &allow-other-keys)
  (declare (ignore status-code))
  (call-next-method))


