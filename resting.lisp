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

(defun add-direct-superclass-test (class superclass)
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


;; DEFROUTE macro
;; 
(define-condition no-such-route (simple-error) ())
(define-condition http-condition (simple-error)
  ((status-code :initarg :code :initform (error "Must supply a HTTP status code.")
                :accessor code)))
(define-condition |404| (http-condition)
  () (:default-initargs :code 404))

(defmethod print-object ((c http-condition) s)
  (print-unreadable-object (c s :type nil :identity nil)
      (format s "HTTP condition ~a" (code c))))

(defmacro defroute (name-or-name-and-qualifier (verb-spec type-spec &rest args) &body body)
  (let* ((name-and-qualifier (alexandria:ensure-list name-or-name-and-qualifier))
         (name (first name-and-qualifier))
         (qualifier (second name-and-qualifier))
         (verb-spec (verb-spec-or-lose verb-spec))
         (type-spec (content-type-spec-or-lose type-spec (second verb-spec))))
    `(progn
       (defmethod ,name ,@(and (keywordp qualifier) (list qualifier))
         (,verb-spec ,type-spec ,@args)
         ;; FIXME: only do this if the reply should take a body
         ;; (i.e. is a GET)
         (setf (hunchentoot:content-type* hunchentoot:*reply*)
               ,(string-downcase (second type-spec)))
         (funcall #'(lambda () ,@body)))
       (defmethod no-applicable-method ((f (eql (function ,name))) &rest args)
         (error 'no-such-route
                :format-control "No applicable route ~%  ~a~%when called with args ~%  ~a" 
                :format-arguments (list f args)))
       (setf (get ',name 'resting::route) t))))

;; (defroute picture (:GET "image/jpeg" user-id album-id) bla)
;; (defroute (picture :before) (:GET "image/jpeg" user-id album-id) bla)
;; (defroute :around (verb type user-id album-id) bla)

(defclass rest-acceptor (hunchentoot:acceptor)
  ((route-packages :initform (list *package*) :initarg :route-packages :accessor route-packages
                   :documentation
                   "A list of packages where generic functions whose
                   names name REST resource exist for the acceptor. ")
   (method-regexp :initform "/([^/]+)(?:/(.*))?" :accessor method-regexp
                  :documentation
                  "A regular expression that when applied to an URI
                  path should contain two capturing subgroups, one for
                  the REST resource and another for the arguments to
                  pass to the verb."))
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
  (let* ((method-regexp (method-regexp acceptor))
         (method-and-rest
           (scan-to-strings* method-regexp uri))
         (method-name (first method-and-rest))
         (actual-arguments (parse-args-in-uri (second method-and-rest))))
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

;; (parse-accept-header "text/html, bla/ble, text/*, */*")(RESTING-TYPES:TEXT/HTML RESTING-TYPES::SEND-ANY-TEXT RESTING-TYPES:SEND-ANYTHING)

(defun parse-content-type-header (string)
  "Return a symbol designating a RESTING-SEND-TYPE object."
  (type-designator-to-type-1 string))

(defmethod hunchentoot:acceptor-dispatch-request ((acceptor rest-acceptor) request)
  (let* ((accept-designators (parse-accept-header (hunchentoot:header-in :accept request)))
         (content-type-designator (parse-content-type-header (hunchentoot:header-in :content-type request)))
         (verb-designator (find-class-1
                           (intern (string-upcase (hunchentoot:request-method request))
                                   :resting-verbs)))
         ;; FIXME: maybe use singletons here
         (verb (and verb-designator
                    (make-instance verb-designator))) 
         (method-and-args
           (multiple-value-list
            (parse-uri (hunchentoot:script-name request) acceptor)))
         (route-method (car method-and-args)))
    (if (and route-method verb)
        (handler-bind ((http-condition
                         #'(lambda (c)
                             (when hunchentoot:*catch-errors-p*
                               (setf (hunchentoot:return-code*) (code c))
                               (hunchentoot:abort-request-handler)))))
          (etypecase verb
            ;; For the Accept: header
            (resting-verbs:sending-verb
             (let ((try-list accept-designators)
                   (retval))
               (loop do (unless try-list
                          (error 'no-such-route
                                 :format-control
                                 "No matching routes found for ~a"
                                 :format-arguments
                                 (list accept-designators)))
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
                                        (second method-and-args)))
                           t)))
               retval))
            (resting-verbs:receiving-verb
             (apply route-method
                    verb
                    (make-instance (or content-type-designator
                                       'resting-types:content)
                                   :content-stream 'something)
                    (second method-and-args)))))
        (call-next-method))))

(defmethod hunchentoot:acceptor-status-message ((acceptor rest-acceptor) status-code &key
                                                &allow-other-keys)
  (declare (ignore status-code))
  (call-next-method))


