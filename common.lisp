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

(defun destructive-p (verb) (typep verb 'snooze-verbs:receiving-verb))


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
                                     (first (scan-to-strings* "([^/]+)" type-designator))))
  (let* ((type (intern-safe type-designator :snooze-types))
         (supertype (intern-safe supertype-designator :snooze-types)))
    `(progn
       (setf (get ',type 'name) ,(string-downcase (symbol-name type)))
       (unless (find-class ',supertype nil)
         (setf (get ',supertype 'name) ,(format nil "~a/*"
                                               (string-downcase (symbol-name supertype))))
         (defclass ,supertype (snooze-types:content) ()))
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
  (string (closer-mop:generic-function-name resource)))

(defun find-resource (designator &optional errorp)
  (cond ((or (stringp designator)
             (keywordp designator))
         (find designator *all-resources* :key #'resource-name :test #'string-equal))
        ((resource-p designator)
         designator)
        ((and designator
              (symbolp designator)
              (fboundp designator)
              (resource-p (symbol-function designator)))
         (symbol-function designator))
        (errorp
         (error "~a doesn't designate a RESOURCE" designator))))

(defmethod initialize-instance :after ((gf resource-generic-function) &rest args)
  (declare (ignore args))
  (pushnew gf *all-resources*))

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
              "The NIL defaults to a genpath-function's &OPTIONALs must be at the end")
        (error "The NILs to a genpath-function's &OPTIONALs must be at the end")))))

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
      `(defun ,genpath-fn-name
           ,@`(;; Nasty, this could easily be a function.
               ;; 
               (,@required
                &optional
                  ,@augmented-optional
                  ,@(if rest
                        (warn 'style-warning
                              :format-control "&REST ~a is not supported for genpath-functions"
                              :format-arguments (list rest)))
                &key
                  ,@all-kwargs
                  ,@(if aok-p `(&allow-other-keys)))
               ;; And at runtime...
               ;;
               (check-optional-args ,optional-args-form)
               (let* ((required-args-list ,required-args-form)
                      (required-part (format nil "/~{~a~^/~}" required-args-list))
                      (optional-args-list (remove nil ,optional-args-form))
                      (optional-part (and optional-args-list
                                          (format nil "/~{~a~^/~}" optional-args-list)))
                      (flattened-keywords-list ,keyword-arguments-form)
                      (query-part (and flattened-keywords-list
                                       (format nil "?~{~a=~a~^&~}" flattened-keywords-list))))
                 (let ((string (format nil "~a~a~a~a"
                                       (string-downcase ',resource-sym)
                                       (or required-part "")
                                       (or optional-part "")
                                       (or query-part ""))))
                   string)))))))

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
           (list (first verb-spec) (verb-designator-to-verb (second verb-spec))))
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

(defun parse-args-in-uri (args-string query fragment)
  (let* ((after-/ (mismatch "/" args-string))
         (plain-args (and after-/
                          (cl-ppcre:split "/" (subseq args-string after-/))))
         (keyword-args (and query
                            (loop for maybe-pair in (cl-ppcre:split "[;&]" query)
                                  for (key-name value) = (scan-to-strings* "(.*)=(.*)" maybe-pair)
                                  when (and key-name value)
                                    append (list (intern (string-upcase key-name) :keyword)
                                                 value)))))
    (values plain-args
            (append keyword-args
                    (when fragment
                      (list 'snooze:fragment fragment))))))

(defun parse-resource (uri-path)
  "Parse URI-PATH for a resource and how it should be called.

Honours of *RESOURCE-NAME-REGEXP*, *ALL-RESOURCES* and
*HOME-RESOURCE*.

Returns nil if the resource cannot be found, otherwise returns up to 4
values: RESOURCE, PLAIN-ARGS, KEYWORD-ARGS and
EXT-CONTENT-TYPE. RESOURCE is a generic function verifying RESOURCE-P.
PLAIN-ARGS is a list of unconverted argument values that the
user-agent wants to pass to the function before any keyword
arguments. KEYWORD-ARGS is a plist of keys and unconverted argument
values that user agent wants to pass to the function as keyword
arguments. EXT-CONTENT-TYPE is a subclass of SNOOZE-TYPES:CONTENT
discovered from the uri \"file extension\" bit."
  ;; <scheme name> : <hierarchical part> [ ? <query> ] [ # <fragment> ]
  ;;
  (let* ((uri (puri:parse-uri uri-path))
         (script-name (puri:uri-path uri))
         (match (multiple-value-list (cl-ppcre:scan *resource-name-regexp*
                                                    script-name)))
         (resource-name
           (and (first match)
                (apply #'subseq script-name
                       (if (plusp (length (third match)))
                           (list (aref (third match) 0) (aref (fourth match) 0))
                           (list (first match) (second match))))))
         (first-slash-resource
           (find-resource resource-name))
         (resource (if resource-name
                       first-slash-resource
                       (find-resource *home-resource*)))
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
                                   (gethash extension *mime-type-hash*))))
         (plain-and-keyword-args (multiple-value-list
                                  (parse-args-in-uri (if content-type-class
                                                         args-string
                                                         (if (zerop (length args-string))
                                                             ""
                                                             script-minus-resource))
                                                     (puri:uri-query uri)
                                                     (puri:uri-fragment uri)))))
    (values resource
            (first plain-and-keyword-args)
            (second plain-and-keyword-args)
            content-type-class)))

(defun content-classes-in-accept-string (string)
  (labels ((expand (class)
             (cons class
                   (reduce #'append (mapcar #'expand (closer-mop:class-direct-subclasses class))))))
    (loop for media-range-and-params in (cl-ppcre:split "\\s*,\\s*" string)
          for media-range = (first (scan-to-strings* "([^;]*)" media-range-and-params))
          for class = (find-content-class media-range)
          when class
            append (expand class))))

(defun arglist-compatible-p (resource args)
  (handler-case
      ;; FIXME: evaluate this need for eval, for security reasons
      (let ((*read-eval* nil))
        (handler-bind ((warning #'muffle-warning))
          (eval `(apply (lambda ,(closer-mop:generic-function-lambda-list
                                  resource)
                          t)
                        '(t t ,@args)))))
    (error () nil)))

(defun parse-content-type-header (string)
  "Return a symbol designating a SNOOZE-SEND-TYPE object."
  (find-content-class string))

(defun find-verb-or-lose (designator)
  (let ((class (or (probe-class-sym
                    (intern (string-upcase designator)
                            :snooze-verbs))
                   (error "Can't find HTTP verb for designator ~a!" designator))))
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

(defmethod explain-condition (condition resource (content-type (eql 'failsafe)))
  (declare (ignore resource))
  (format nil "~a" condition))

(defmethod explain-condition (condition resource (content-type (eql 'full-backtrace)))
  (declare (ignore resource))
  (format nil "Your SNOOZE was bitten by:~&~a"
          (with-output-to-string (s)
            (uiop/image:print-condition-backtrace condition :stream s))))

(defmethod explain-condition (condition resource (content-type snooze-types:text/plain))
  (explain-condition condition resource 'failsafe))

(define-condition http-condition (simple-condition)
  ((status-code :initarg :status-code :initform (error "Must supply a HTTP status code.")
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

(define-condition  unsupported-content-type (http-error) ()
  (:default-initargs
   :status-code 501
   :format-control "Resource exists but invalid arguments passed"))

(define-condition no-such-route (http-condition) ()
  (:default-initargs
   :format-control "Resource exists but no such route"))


(defmethod initialize-instance :after ((e http-error) &key)
  (assert (<= 500 (status-code e) 599) nil
          "An HTTP error must have a status code between 500 and 599"))

(defmethod convert-arguments (resource plain-arguments keyword-arguments)
  (declare (ignore resource))
  (flet ((probe (value)
           (or (let ((*read-eval* nil))
                 (ignore-errors
                  (read-from-string value)))
               value)))
    (values
     (mapcar #'probe plain-arguments)
     (loop for (key value) on keyword-arguments by #'cddr
           collect key
           collect (probe value)))))

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
                       (content-class-name 'text/plain)))))
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
                         (content-class-name accepted-type)))))
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

(defvar *resource*
  "Bound early in HANDLE-REQUEST-1 to nil or to a RESOURCE.
Used by POLITELY-EXPLAINING-CONDITIONS and
BRUTALLY-EXPLAINING-CONDITIONS to pass a resource to
EXPLAIN-CONDITION.")

(defun handle-request-1 (uri method accept content-type)
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
            (multiple-value-bind (converted-plain-args converted-keyword-args)
                (convert-arguments *resource* plain-args keyword-args)
              (let ((converted-arguments (append converted-plain-args converted-keyword-args)))
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
                                             (content-class-name payload-ct)))))))))))))

(defmethod print-object ((c http-condition) s)
  (print-unreadable-object (c s :type t)
    (format s "~a" (status-code c))))
