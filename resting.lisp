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

(defpackage :resting-recv-types (:use) (:export #:anything))

(defpackage :resting-send-types (:use) (:export #:anything))

(defclass resting-recv-types:anything ()
  ((content-stream :initarg :content-stream)))

(defclass resting-send-types:anything () ())

(defun intern-safe (designator package)
  (intern (string-upcase designator) package))

(defmacro define-recv-type (type-designator supertype-designator)
  (let ((type (intern-safe type-designator :resting-recv-types))
        (supertype (intern-safe supertype-designator :resting-recv-types)))
    `(progn
       ,@(unless (find-class supertype nil)
           `((defclass ,supertype (resting-recv-types:anything) ())))
       (defclass ,type (,supertype) ())
       (export '(,type ,supertype) :resting-recv-types))))

(defmacro define-send-type (type-designator supertype-designator)
  (let ((type (intern-safe type-designator :resting-send-types))
        (supertype (intern-safe supertype-designator :resting-send-types)))
    (labels ((supertype-superclasses (supertype)
               (let ((retval))
                 (do-symbols (sym :resting-send-types)
                   (when (and (eql (length (symbol-name supertype))
                                   (mismatch (symbol-name supertype)
                                             (symbol-name sym)))
                              (find-class sym nil))
                     (push sym retval)))
                 retval)))
      `(progn
         (defclass ,type () ())
         (defclass ,supertype ,(supertype-superclasses supertype) ())
         (export '(,type ,supertype) :resting-send-types)))))

(defmacro define-known-content-types ()
  `(progn
     ,@(loop for (type-spec . nil) in hunchentoot::*mime-type-list*
             for matches = (nth-value 1 (cl-ppcre:scan-to-strings "((.*)/(.*))(;.*)?" type-spec))
             for type = (and matches (aref matches 0))
             for supertype = (and matches (aref matches 1))
             when supertype
               collect `(define-recv-type ,type ,supertype)
               and collect `(define-send-type ,type ,supertype))))


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

(defun catch-all-type (package)
  (intern "ANYTHING" package))

(defun type-designator-to-type-1 (designator package)
  "Return DESIGNATOR as class-defining symbol in PACKAGE, or nil."
  (or (and (eq designator t)
           (progn
             (alexandria:simple-style-warning
              "Converting content-type designator T to ~a"
              (catch-all-type package))
             (catch-all-type package)))
      (find-class-1 (intern (string-upcase designator) package))
      (and (string= designator "*/*")
           (catch-all-type package))
      (let* ((matches (nth-value 1
                                 (cl-ppcre:scan-to-strings
                                  "([^/]+)/\\*"
                                  designator)))
             (supertype-designator (and matches
                                        (aref matches 0))))
        (find-class-1 (intern (string-upcase supertype-designator)
                              package)))))

(defun content-type-spec-or-lose (type-spec verb)
  (labels ((type-designator-to-type (designator)
             (or 
              (cond ((subtypep verb 'resting-verbs:receiving-verb)
                     (type-designator-to-type-1 designator :resting-recv-types))
                    ((subtypep verb 'resting-verbs:sending-verb) 
                     (type-designator-to-type-1 designator :resting-send-types))
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

;; (content-type-spec-or-lose '(foo t) 'resting-verbs:get)     ; (FOO RESTING-SEND-TYPES:ANYTHING)
;; (content-type-spec-or-lose '(foo "*/*") 'resting-verbs:get) ; (FOO RESTING-SEND-TYPES:ANYTHING)
;; (content-type-spec-or-lose 'foo 'resting-verbs:get)         ; (FOO RESTING-SEND-TYPES:ANYTHING)
;;  
;; (content-type-spec-or-lose '(foo t) 'resting-verbs:put)     ; (FOO RESTING-RECV-TYPES:ANYTHING)
;; (content-type-spec-or-lose '(foo "*/*") 'resting-verbs:put) ; (FOO RESTING-RECV-TYPES:ANYTHING)
;; (content-type-spec-or-lose '(foo "text/*") 'resting-verbs:put) ; (FOO RESTING-RECV-TYPES:TEXT)
;; (content-type-spec-or-lose '(foo "text/html") 'resting-verbs:put) ; (FOO RESTING-RECV-TYPES:TEXT/HTML)
;; (content-type-spec-or-lose "text/html" 'resting-verbs:put) ; (TYPE RESTING-RECV-TYPES:TEXT/HTML)
;; (content-type-spec-or-lose '"text/html" 'resting-verbs:put) ; (TYPE RESTING-RECV-TYPES:TEXT/HTML)
;; (content-type-spec-or-lose '"text/html-typo" 'resting-verbs:put) ; error!
;; (content-type-spec-or-lose 'foo 'resting-verbs:put); (FOO RESTING-RECV-TYPES:ANYTHING)
;; (content-type-spec-or-lose '(foo resting-recv-types:text/html) 'resting-verbs:put) ; (FOO RESTING-RECV-TYPES:TEXT/HTML)
;; (content-type-spec-or-lose '(foo resting-recv-types:text-html-typo) 'resting-verbs:put) ; error!
;; (content-type-spec-or-lose '(foo resting-send-types:text/html) 'resting-verbs:put); (FOO RESTING-RECV-TYPES:TEXT/HTML) ! autocorrect!

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
    `((defmethod no-applicable-method ((f (eql (function ,name))) &rest ignored)
        (declare (ignore ignored))
        (error 'no-such-route))
      (defmethod ,name ,@(and (keywordp qualifier) (list qualifier))
        (,verb-spec ,type-spec ,@args)
        ,@body)
      (setf (get ,name 'resting::route) t))))

;; (defroute picture (:GET "image/jpeg" user-id album-id) bla)
;; (defroute (picture :before) (:GET "image/jpeg" user-id album-id) bla)
;; (defroute :around (verb type user-id album-id) bla)



(defclass rest-acceptor (hunchentoot:acceptor)
  ((route-packages :initform (list *package*) :initarg :route-packages :accessor route-packages))
  (:documentation "An acceptor for RESTful routes"))


(defparameter *default-send-type* 'resting-send-types:text/html)
(defparameter *default-recv-type* 'resting-recv-types:anything)





;;; Helpers for our HUNCHENTOOT:ACCEPTOR-DISPATCH-REQUEST method
;;;
;;; FIXME: very naive, need lots of work
(defun parse-uri (uri)
  (cl-ppcre:split "/" uri :start 1))

(defun parse-accept-header (string)
  "Return a list of symbols designating RESTING-RECV-TYPE objects.
In the correct order" 
  (cl-ppcre:split "," string))

(defun parse-content-type-header (string)
  "Return a symbol designating a RESTING-SEND-TYPE object."
  (type-designator-to-type-1 string :resting-recv-types))

(defmethod hunchentoot:acceptor-dispatch-request ((acceptor rest-acceptor) request)
  (let* ((accept-designators (parse-accept-header (hunchentoot:header-in :accept request)))
         (content-type (parse-content-type-header (hunchentoot:header-in :content-type request)))
         (verb (find-class (intern (string-upcase (hunchentoot:request-method request)))))
         (uri-and-args (parse-uri (hunchentoot:script-name request)))
         (route-method (loop for package in (route-packages acceptor)
                               thereis (find-symbol (car uri-and-args) package))))
    (etypecase verb
      ;; For the Accept: header
      (resting-verbs:sending-verb
       (loop for (type . more) on accept-designators
             do
                (block try-again
                  (handler-bind ((no-such-route
                                   #'(lambda (c)
                                       (declare (ignore c))
                                       (if more
                                           (return-from try-again)))))
                    (apply route-method
                           (make-instance verb)
                           (make-instance type)
                           (cdr uri-and-args))))))
      (resting-verbs:receiving-verb
       (apply route-method
              (make-instance verb)
              (make-instance (or content-type
                                 'resting-recv-types:anything)
                             :content-stream 'something)
              (cdr uri-and-args))))))

(defmethod hunchentoot:acceptor-status-message ((acceptor rest-acceptor) status-code &key
                                                &allow-other-keys)
  (call-next-method))

;; (find-route :get "application/json" "/books/123")

;; (defroute :get "application/json" "^/books/([0-9]+)$"
;;     (id)
;;   (json:encode-json-to-string (list 1 6 2 (parse-integer id))))





