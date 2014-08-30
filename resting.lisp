(in-package #:resting)

(defpackage :resting-routes)
(defparameter *default-content-type* "text/html")

(defclass rest-acceptor (hunchentoot:acceptor)
  ()
  (:documentation "An acceptor for RESTful routes"))

(defclass route ()
  ((verb :initarg :verb :accessor route-verb)
   (content-type :initform nil :initarg :content-type :accessor route-content-type)
   (regexp :initarg :regexp :accessor route-regexp)
   (handler :initarg :handler :accessor route-handler)))

(defun make-regexp (name args)
  (format nil "^/~a~{/(~*[^/]+)~}/?" name args))

(defmacro defroute (name-or-name-and-options args &body body)
  (let* ((name-and-options (alexandria:ensure-list name-or-name-and-options))
         (name (car name-and-options)))
    `(let ((fn #'(lambda ,args ,@body)))
       (values
        (defun ,name ,args
          ;; FIXME: use parse-ordinary-lambda-list
          (funcall fn ,@args))
        (setf
         (get (intern ,(symbol-name name) :resting-routes) 'route)
         (make-instance 'route
                        :verb :get
                        :regexp ,(make-regexp (string-downcase name) args)
                        :handler ',name
                        ,@(cdr name-and-options)))))))

(defmethod print-object ((route route) stream)
  (print-unreadable-object (route stream :type t)
    (format stream "~a:~a:~a" (route-verb route) (route-content-type route) (route-regexp route))))

(defun match-content-type-p (supported proposed)
  ;; FIXME: improve
  (or (string= proposed "*/*")
      (string= supported proposed)))

(defun match-route-1 (route verb type uri)
  (eq verb (route-verb route))
  (match-content-type-p (or (route-content-type route)
                            *default-content-type*) type)
  (nth-value 1 (cl-ppcre:scan-to-strings
                (route-regexp route)
                uri)))

(defun match-route (verb type uri)
  (do-symbols (sym :resting-routes)
    (let*
        ((route (get sym 'route))
         (probe (cond
                  ((not route)
                   (warn "Oopps symbol ~a didn't have any route attached" sym))
                  ((not (fboundp (route-handler route)))
                   (warn "Route handler ~a not fbound, you probably messed up..."
                         (route-handler route)))
                  ((and route
                        (match-route-1 route verb type uri))))))
      (when probe
        (return (cons route probe))))))

(defmethod hunchentoot:acceptor-dispatch-request ((acceptor rest-acceptor) request)
  (let* ((accept (hunchentoot:header-in :accept request))
         (accepted-types (and accept (cl-utilities:split-sequence #\, accept)))
         (content-type (hunchentoot:header-in :content-type request))
         (verb (hunchentoot:request-method request))
         (uri (hunchentoot:script-name request))
         (route-and-args (cond ((member verb '(:get))
                                (some #'(lambda (type)
                                          (match-route verb type uri))
                                      accepted-types))
                               (t
                                (match-route verb content-type uri)))))
    (if route-and-args
        (apply (route-handler (car route-and-args))
               (map 'list #'identity (cdr route-and-args)))
        (call-next-method))))

(defmethod hunchentoot:acceptor-status-message ((acceptor rest-acceptor) status-code &key
                                                &allow-other-keys)
  (call-next-method))

;; (find-route :get "application/json" "/books/123")

;; (defroute :get "application/json" "^/books/([0-9]+)$"
;;     (id)
;;   (json:encode-json-to-string (list 1 6 2 (parse-integer id))))





