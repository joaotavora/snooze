(in-package #:resting)

(defpackage :resting-recv-types (:use) (:export #:anything))
(defpackage :resting-send-types (:use) (:export #:anything))
(defpackage :resting-verbs (:use) (:export #:get #:post #:put #:delete))

(defclass resting-recv-types:anything () ())
(defclass resting-send-types:anything () ())

(defun intern-safe (designator package)
  (intern (string-upcase (string designator)) package))

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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-known-content-types))

(defun content-type-or-lose (designator package)
  (let ((sym
          (find-symbol (string-upcase (string designator))
                       package)))
    (unless (find-class sym nil)
      (error "There is no HTTP Content-Type of the ~a kind designated by ~a"
             package designator))
    sym))

(defun send-type-or-lose (designator)
  (content-type-or-lose designator :resting-send-types))

(defun recv-type-or-lose (designator)
  (content-type-or-lose designator :resting-recv-types))



(defun http-verb-or-lose (thing)
  (let ((sym
          (find-symbol (string-upcase (string thing))
                       :resting-verbs)))
    (or sym
        (error "No such \"~a\" HTTP verb" thing))))

(defclass rest-acceptor (hunchentoot:acceptor)
  ()
  (:documentation "An acceptor for RESTful routes"))


(defparameter *default-send-type* 'resting-send-types:text/html)
(defparameter *default-recv-type* 'resting-recv-types:anything)

(define-condition no-such-route (simple-error) ())

(defmacro defroute (name-or-name-and-options args &body body)
  (let* ((name-and-options (alexandria:ensure-list name-or-name-and-options))
         (name (car name-and-options))
         (options (cdr name-and-options))
         (verb-designator (getf options :verb))
         (verb (or (and verb-designator
                        (http-verb-or-lose verb-designator))
                   'resting-verbs:get))
         (content-designator (getf options :content-type))
         (type
           (ecase verb
             ;; For the Accept: header
             (resting-verbs:get (or (and content-designator
                                         (content-type-or-lose content-designator :resting-send-types))
                                    *default-send-type*))
             ;; For the Content-Type: header
             ((resting-verbs:put
               resting-verbs:post)
              (or (and content-designator
                       (content-type-or-lose content-designator :resting-recv-types))
                  *default-recv-type*))
             (resting-verbs:delete
              ;; There can be no content in either direction.
              (assert (not content-designator) nil
                      "For HTTP DELETE, Content-Type doesn't make sense")))))
    `((defmethod no-applicable-method ((f (eql (function ,name))) &rest ignored)
        (declare (ignore ignored))
        (error 'no-such-route))
      (defmethod ,name ((resting::verb ,verb)
                        (resting::type ,type)
                        ,@args)
        ,@body))))

(defun parse-uri (uri)
  ;; FIXME needs lots of work
  (cl-ppcre:split "/" uri :start 1))


(defmethod hunchentoot:acceptor-dispatch-request ((acceptor rest-acceptor) request)
  (let* ((accept (hunchentoot:header-in :accept request))
         (accepted-types (or (and accept (cl-ppcre:split "," accept))
                             'resting-send-types:anything))
         (content-type (hunchentoot:header-in :content-type request))
         (verb-designator (hunchentoot:request-method request))
         (verb (http-verb-or-lose verb-designator))
         (uri (hunchentoot:script-name request))
         ;; FIXME needs a lot of work
         (uri-and-args (parse-uri uri)))
    (when (and (eq 'resting-verbs:get verb)
               content-type)
      (error "Sorry, no \"Content-Type:\" allowed for HTTP GET"))
    (when (and (member verb '(resting-verbs:post resting-verbs:put))
               accepted-types)
      (error "Sorry, no \"Accept:\" header allowed for HTTP GET"))
    (ecase verb
      ;; For the Accept: header
      (resting-verbs:get
       (when content-type
         (error "Sorry, no \"Content-Type:\" allowed for HTTP GET"))
       (loop for type in (mapcar #'send-type-or-lose accepted-types)
             do (apply (car uri-and-args)
                       (make-instance verb)
                       (make-instance type)
                       (cdr uri-and-args)))
      ((resting-verbs:put
        resting-verbs:post
        resting-verbs:delete)
       (apply (car uri-and-args)
              (make-instance verb)
              (make-instance (recv-type-or-lose content-type))
              (cdr uri-and-args)))))))

(defmethod hunchentoot:acceptor-status-message ((acceptor rest-acceptor) status-code &key
                                                &allow-other-keys)
  (call-next-method))

;; (find-route :get "application/json" "/books/123")

;; (defroute :get "application/json" "^/books/([0-9]+)$"
;;     (id)
;;   (json:encode-json-to-string (list 1 6 2 (parse-integer id))))





