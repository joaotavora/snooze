(in-package #:snooze-common)


;;; Verbs
;;;
;;; "Sending" and "Receiving" are always from the server's
;;; perspective. Hence GET is "sending to client" and POST and PUT are
;;; "receiving from client".
;;; 
(defpackage :snooze-verbs (:use) (:export #:http-verb #:get #:post #:put #:delete
                                          #:content-verb
                                          #:receiving-verb
                                          #:sending-verb))

(cl:defclass snooze-verbs:http-verb      () ())
(cl:defclass snooze-verbs:delete         (snooze-verbs:http-verb) ())
(cl:defclass snooze-verbs:content-verb   (snooze-verbs:http-verb) ())
(cl:defclass snooze-verbs:receiving-verb (snooze-verbs:content-verb) ())
(cl:defclass snooze-verbs:sending-verb   (snooze-verbs:content-verb) ())
(cl:defclass snooze-verbs:post           (snooze-verbs:receiving-verb) ())
(cl:defclass snooze-verbs:put            (snooze-verbs:receiving-verb) ())
(cl:defclass snooze-verbs:get            (snooze-verbs:sending-verb) ())


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
     ,@(loop for (type-spec . nil) in *mime-type-list*
             for matches = (nth-value 1 (cl-ppcre:scan-to-strings "(.*/.*)(?:;.*)?" type-spec))
             for type = (and matches (aref matches 0))
             when type
               collect `(define-content ,type))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-known-content-types))


;;; Helpers
;;; 
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
                      (required-part (format nil "/~{~a~^/~}" required-args-list))
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
                 (probe-class-sym (intern (string-upcase designator)
                                          :snooze-verbs))
                 (error "Sorry, don't know the HTTP verb ~a"
                        (string-upcase designator)))))
    (cond ((and verb-spec
                (listp verb-spec))
           (list (first verb-spec) (verb-designator-to-verb (second verb-spec))))
          ((or (keywordp verb-spec)
               (stringp verb-spec))
           (list 'snooze-verbs:http-verb (verb-designator-to-verb verb-spec)))
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

