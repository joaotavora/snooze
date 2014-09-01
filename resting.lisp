(in-package #:resting)


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
;;; Apropos the distinction between normal and "send-any" types, note
;;; that in GET requests we are only interested in the request's
;;; "Accept" header, since GET never have useful bodies (1) and as
;;; such don't have "Content-Type".
;;;
;;; [1]: http://stackoverflow.com/questions/978061/http-get-with-request-body
;;;
;;; So, for GET requests, we make a inverted hierarchy of types for
;;; dispatching on the various types of the "Accept:" header. We do
;;; this using MOP.
;;;
(cl:in-package :resting)
;; (delete-package :resting-types)
(defpackage :resting-types (:use) (:export #:send-anything #:content))

(eval-when (:compile-toplevel :load-toplevel :execute)

  (defclass resting-types:content ()
    ((content-stream :initarg :content-stream)))
  
  (defclass resting-types:send-anything (resting-types:content) ())
  
  (defun intern-safe (designator package)
    (intern (string-upcase designator) package))
  (defun send-any-symbol (supertype)
    (intern (string-upcase (format nil "SEND-ANY-~a" supertype))
            :resting-types)))

(defun add-direct-superclass (class superclass)
  (closer-mop:add-direct-subclass superclass class)
  (closer-mop:ensure-class (class-name class)
                           :direct-superclasses
                           (remove-duplicates
                            (cons superclass
                                  (closer-mop:class-direct-superclasses class)))))

(defmacro define-content (type-designator supertype-designator)
  (let ((type (intern-safe type-designator :resting-types))
        (supertype (intern-safe supertype-designator :resting-types))
        (send-any-supertype (send-any-symbol supertype-designator)))
    `(progn
       (unless (find-class ',supertype nil)
         (defclass ,supertype (resting-types:content) ()))
       (unless (find-class ',send-any-supertype nil)
         (defclass ,send-any-supertype () ()))
       (defclass ,type (,supertype) ())
       (eval-when (:compile-toplevel :load-toplevel :execute)
         (export '(,type ,supertype ,send-any-supertype) :resting-types))
       (add-direct-superclass (find-class ',send-any-supertype) (find-class ',type))
       (add-direct-superclass (find-class 'resting-types:send-anything)
                              (find-class ',send-any-supertype)))))

(defmacro define-known-content-types ()
  `(progn
     ,@(loop for (type-spec . nil) in hunchentoot::*mime-type-list*
             for matches = (nth-value 1 (cl-ppcre:scan-to-strings "((.*)/(.*))(;.*)?" type-spec))
             for type = (and matches (aref matches 0))
             for supertype = (and matches (aref matches 1))
             when (and type supertype)
               collect `(define-content ,type ,supertype))))


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

(defun type-designator-to-type-1 (designator &optional for-sending)
  "Return DESIGNATOR as content-type class-defining symbol or nil."
  (or (and (eq designator t)
           (progn
             (alexandria:simple-style-warning
              "Coercing content-designating type designator T to ~s"
              'resting-types:content)
             'resting-types:content))
      (find-class-1 (intern (string-upcase designator) :resting-types))
      (and (string= designator "*/*")
           (if for-sending
               'resting-types:send-anything
               'resting-types:content))
      (let* ((matches (nth-value 1
                                 (cl-ppcre:scan-to-strings
                                  "([^/]+)/\\*"
                                  (string-upcase designator))))
             (supertype-designator (and matches
                                        (aref matches 0))))
        (find-class-1
         (if for-sending
             (send-any-symbol supertype-designator)
             (intern (string-upcase supertype-designator) :resting-types))))))

(defun content-type-spec-or-lose-1 (type-spec)
  (labels ((type-designator-to-type (designator)
             (or 
              (type-designator-to-type-1 designator)
              (error "Sorry, don't know the content-type ~a" type-spec))))
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


;; Conditions
;; 
(define-condition http-condition (simple-error)
  ((status-code :initarg :code :initform (error "Must supply a HTTP status code.")
                :accessor code)))
(define-condition no-such-route (http-condition)
  () (:default-initargs :code 404))
(define-condition |404| (http-condition)
  () (:default-initargs :code 404))

(defmethod print-object ((c http-condition) s)
  (print-unreadable-object (c s :type nil :identity nil)
    (format s "HTTP condition ~a" (code c))))

(defgeneric explain-condition (acceptor c &rest)
  (:documentation
   "In the context of ACCEPTOR, explain exceptional condition C to client.
This function can/should set HUNCHENTOOT:CONTENT-TYPE* on the current
reply. CLIENT-ACCEPT-DESIGNATORS is a list of symbols designating
subclasses of RESTING-TYPES:CONTENT that the client wanted, in case
you want to use it."))

(defmethod explain-condition (acceptor c &key client-accept-designators)
  (declare (ignore acceptor client-accept-designators))
  (setf (hunchentoot:content-type*) "text/plain")
  (format nil "~?" (simple-condition-format-control c)
          (simple-condition-format-arguments c)))


;; DEFROUTE macro
;; 
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
       (defmethod ,name ,@qualifiers
         ,proper-lambda-list
         ,@(if docstring `(,docstring))
         ,@declarations
         ,@remaining)
       (defmethod no-applicable-method ((f (eql (function ,name))) &rest args)
         (error 'no-such-route
                :format-control "No applicable route ~%  ~a~%when called with args ~%  ~a" 
                :format-arguments (list f args)))
       (setf (get ',name 'argchecker-dummy)
             #'(lambda ,simplified-lambda-list
                 (declare (ignore ,@(remove-if #'(lambda (sym)
                                                   (eq #\& (aref (symbol-name sym) 0)))
                                               simplified-lambda-list))))))))

;; (defroute picture (:GET "image/jpeg" user-id album-id) bla)
;; (defroute (picture :before) (:GET "image/jpeg" user-id album-id) bla)
;; (defroute :around (verb type user-id album-id) bla)

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



;;; Helpers for our HUNCHENTOOT:ACCEPTOR-DISPATCH-REQUEST method
;;;
;;; FIXME: very naive, need lots of work
;;;
(defun scan-to-strings* (regex string)
  (coerce (nth-value 1
                  (cl-ppcre:scan-to-strings regex
                                            string))
          'list))

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

(defun parse-accept-header (string)
  "Return a list of symbols designating RESTING-RECV-TYPE objects.
In the correct order" 
  (loop for a in (cl-ppcre:split ",\\s*" string)
        when (type-designator-to-type-1 a 'for-sending)
          collect it))

(defun arglist-compatible-p (method args)
  (let ((checker (get method 'argchecker-dummy)))
    (cond ((null checker) t)
          (t
           (handler-case
               (progn
                 (apply checker
                        (append '(dummyverb dummytype)
                                args))
                 t)
             (error () nil))))))

;; (parse-accept-header "text/html, bla/ble, text/*, */*")(RESTING-TYPES:TEXT/HTML RESTING-TYPES::SEND-ANY-TEXT RESTING-TYPES:SEND-ANYTHING)

(defun parse-content-type-header (string)
  "Return a symbol designating a RESTING-SEND-TYPE object."
  (type-designator-to-type-1 string))

(defmethod hunchentoot:acceptor-dispatch-request ((acceptor rest-acceptor) request)
  (let* ((accept-designators (parse-accept-header (hunchentoot:header-in :accept request)))
         (content-type-designator (parse-content-type-header (hunchentoot:header-in :content-type request)))
         (verb-designator (or (find-class-1
                               (intern (string-upcase (hunchentoot:request-method request))
                                       :resting-verbs))
                              (error "Can't find HTTP verb designator for request ~a!" request)))
         ;; FIXME: maybe use singletons here
         (verb (and verb-designator
                    (make-instance verb-designator))) 
         (method-and-args
           (multiple-value-list
            (parse-uri (hunchentoot:script-name request) acceptor)))
         (route-method (first method-and-args))
         (route-arguments (second method-and-args)))
    (handler-bind ((http-condition
                     #'(lambda (c)
                         ;; FIXME: make this a restart
                         (when hunchentoot:*catch-errors-p*
                           (setf (hunchentoot:return-code*) (code c))
                           (hunchentoot:abort-request-handler
                            (explain-condition acceptor c
                                               :client-accept-designators accept-designators))))))
      (cond ((not route-method)
             (if (fall-through-p acceptor)
                 (call-next-method)
                 (error 'no-such-route
                        :format-control
                        "So sorry, but that URI doesn't match any REST resources")))
            ((not (arglist-compatible-p route-method
                                        (second method-and-args)))
             (error 'no-such-route
                    :format-control
                    "Too many, too few, or unsupported query arguments for REST resource ~a"
                    :format-arguments
                    (list route-method)))
            (t
             (etypecase verb
               ;; For the Accept: header
               (resting-verbs:sending-verb
                (let ((try-list accept-designators)
                      (retval))
                  (loop do (unless try-list
                             (error 'no-such-route
                                    :format-control
                                    "No matching routes for verb ~a on resource ~a and accept list ~a"
                                    :format-arguments
                                    (list verb route-method accept-designators)))
                          thereis
                          (block try-again
                            (handler-bind ((no-such-route
                                             #'(lambda (c)
                                                 (declare (ignore c))
                                                 (return-from try-again nil))))
                              (setq retval
                                    (apply route-method
                                           verb
                                           ;; FIXME: maybe use singletons here
                                           (make-instance (pop try-list))
                                           route-arguments))
                              t)))
                  retval))
               (resting-verbs:receiving-verb
                (apply route-method
                       verb
                       (make-instance (or content-type-designator
                                          'resting-types:content)
                         :content-stream 'something)
                       route-arguments))))))))

(defmethod hunchentoot:acceptor-status-message ((acceptor rest-acceptor) status-code &key
                                                &allow-other-keys)
  (declare (ignore status-code))
  (call-next-method))


