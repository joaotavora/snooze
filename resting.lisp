(in-package #:resting)


;;; Verbs
;;;
;;; "Sending" and "Receiving" are always from the server's
;;; perspective. Hence GET is "sending" and POST and PUT are
;;; "receiving".
;;; 
(defpackage :resting-verbs (:use) (:export #:http-verb #:get #:post #:put #:delete
                                           #:receiving-verb
                                           #:sending-verb))
(in-package :resting-verbs)

(cl:defclass http-verb () ())

(cl:defclass delete (http-verb) ())

(cl:defclass receiving-verb (http-verb) ())
(cl:defclass sending-verb   (http-verb) ())

(cl:defclass post (receiving-verb) ())
(cl:defclass put  (receiving-verb) ())

(cl:defclass get (sending-verb) ())

;;; Content-types
;;;
;;; Apropos the distinction between "receive" and "send" types, note
;;; that in GET requests we are only interested in the request's
;;; "Accept" header, since GET never have useful bodies (1) and as
;;; such don't have "Content-Type".
;;;
;;; [1]: http://stackoverflow.com/questions/978061/http-get-with-request-body
;;;
;;; So, for GET requests, we make a inverted hierarchy of types for
;;; dispatching on the various types of the "Accept:" header.
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
                         "Converting T in ~a to ~a"
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
              "Converting content-type designator T to ~a"
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

(defun content-type-spec-or-lose (type-spec verb)
  (labels ((type-designator-to-type (designator)
             (or 
              (cond ((or (subtypep verb 'resting-verbs:receiving-verb)
                         (subtypep verb 'resting-verbs:sending-verb))
                     (type-designator-to-type-1 designator))
                    (designator
                     ;; In this case, bitch if the designator is not T
                     ;;
                     (assert (eq t designator) nil
                             "For verb ~a, no specializations on Content-Type are allowed"
                             verb)
                     designator))
              (error "Sorry, don't know the content-type ~a" type-spec))))
    (cond ((and type-spec
                (listp type-spec))
           (list (first type-spec) (type-designator-to-type (second type-spec))))
          ((or (keywordp type-spec)
               (stringp type-spec))
           (list 'type (type-designator-to-type type-spec)))
          (type-spec
           (list type-spec (type-designator-to-type t))))))

;; half-assed tests for the DEFROUTE helpers
;;
;; (verb-spec-or-lose '(foo t)) ; (FOO RESTING-VERBS:HTTP-VERB)
;; (verb-spec-or-lose :get)     ; (VERB RESTING-VERBS:GET)
;; (verb-spec-or-lose "GET")    ; (VERB RESTING-VERBS:GET)
;; (verb-spec-or-lose '(v resting-verbs:get)) ;(V RESTING-VERBS:GET)

;; (content-type-spec-or-lose '(foo t) 'resting-verbs:get)     ; (FOO RESTING-TYPES:CONTENT)
;; (content-type-spec-or-lose '(foo "*/*") 'resting-verbs:get) ; (FOO RESTING-TYPES:CONTENT)
;; (content-type-spec-or-lose :text/html 'resting-verbs:get)   ; (TYPE RESTING-TYPES:TEXT/HTML)
;; (content-type-spec-or-lose "text/html" 'resting-verbs:get)  ; (TYPE RESTING-TYPES:TEXT/HTML)
;; (content-type-spec-or-lose :text/* 'resting-verbs:get)      ; (TYPE RESTING-TYPES:TEXT)
;; (content-type-spec-or-lose "text/*" 'resting-verbs:get)     ; (TYPE RESTING-TYPES:TEXT)
;; (content-type-spec-or-lose 'foo 'resting-verbs:get)         ; (FOO RESTING-TYPES:CONTENT)
;;  
;; (content-type-spec-or-lose '(foo t) 'resting-verbs:put)     ; (FOO RESTING-TYPES:CONTENT)
;; (content-type-spec-or-lose '(foo "*/*") 'resting-verbs:put) ; (FOO RESTING-TYPES:CONTENT)
;; (content-type-spec-or-lose '(foo "text/*") 'resting-verbs:put) ; (FOO RESTING-TYPES:TEXT)
;; (content-type-spec-or-lose '(foo "text/html") 'resting-verbs:put) ; (FOO RESTING-TYPES:TEXT/HTML)
;; (content-type-spec-or-lose "text/html" 'resting-verbs:put) ; (TYPE RESTING-TYPES:TEXT/HTML)
;; (content-type-spec-or-lose '"text/html" 'resting-verbs:put) ; (TYPE RESTING-TYPES:TEXT/HTML)
;; (content-type-spec-or-lose '"text/html-typo" 'resting-verbs:put) ; error!
;; (content-type-spec-or-lose 'foo 'resting-verbs:put); (FOO RESTING-TYPES:CONTENT)
;; (content-type-spec-or-lose '(foo resting-types:text/html) 'resting-verbs:put) ; (FOO RESTING-TYPES:TEXT/HTML)
;; (content-type-spec-or-lose '(foo resting-types:text-html-typo) 'resting-verbs:put) ; error!
;; (content-type-spec-or-lose '(foo resting-types:text/html) 'resting-verbs:put); (FOO RESTING-TYPES:TEXT/HTML) ! autocorrects!

;; (content-type-spec-or-lose 'foo 'resting-verbs:http-verb); (FOO T)
;; (content-type-spec-or-lose "text/html" 'resting-verbs:http-verb) ; error!
;; (content-type-spec-or-lose "text/html" 'resting-verbs:delete) ;error!



;; DEFROUTE macro
;; 
(define-condition no-such-route (simple-error) ())

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
  ((route-packages :initform (list *package*) :initarg :route-packages :accessor route-packages))
  (:documentation "An acceptor for RESTful routes"))



;;; Helpers for our HUNCHENTOOT:ACCEPTOR-DISPATCH-REQUEST method
;;;
;;; FIXME: very naive, need lots of work
(defun parse-uri (uri packages)
  (let* ((words (cl-ppcre:split "/" uri :start 1))
         (method-name (or (first words) "root")))
    (cons (loop for package in packages
                  thereis (find-symbol (string-upcase
                                        method-name) package))
          (cdr words))))



;; (parse-uri "/bla/ble") ; => ("bla" "ble")

(defun parse-accept-header (string)
  "Return a list of symbols designating RESTING-RECV-TYPE objects.
In the correct order" 
  (loop for a in (cl-ppcre:split ",\\s*" string)
        when (type-designator-to-type-1 a 'for-sending)
          collect it))

;; (subtypep 'resting-types:send-any-text 'resting-types:text/html) ; => T T
;; (subtypep 'resting-types:send-anything 'resting-types:text/html) ; => T T
;; (subtypep 'resting-types:send-any-text 'resting-types:application/xml) ; => NIL T
;; (find-class 'resting-types:send-anything)

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
         (method-and-args (parse-uri (hunchentoot:script-name request) (route-packages acceptor)))
         (route-method (car method-and-args)))
    (if (and route-method verb)
        (etypecase verb
          ;; For the Accept: header
          (resting-verbs:sending-verb
           (let ((try-list accept-designators))
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
                         (apply route-method
                                verb
                                ;; FIXME: maybe use singletons here
                                (make-instance (pop try-list))
                                (cdr method-and-args)))))))
          (resting-verbs:receiving-verb
           (apply route-method
                  verb
                  (make-instance (or content-type-designator
                                     'resting-types:content)
                                 :content-stream 'something)
                  (cdr method-and-args))))
        (call-next-method))))

(defmethod hunchentoot:acceptor-status-message ((acceptor rest-acceptor) status-code &key
                                                &allow-other-keys)
  (call-next-method))


