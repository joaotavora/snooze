(fiasco:define-test-package :resting-tests
  (:use #:cl #:resting)
  (:import-from  #:resting
                 #:verb-spec-or-lose
                 #:content-type-spec-or-lose-1
                 #:parse-uri
                 #:parse-args-in-uri))
(in-package :resting-tests)

(deftest parse-verbs ()
  (handler-bind ((style-warning #'muffle-warning))
    (is (equal (verb-spec-or-lose '(foo t)) '(FOO RESTING-VERBS:HTTP-VERB)))
    (is (equal (verb-spec-or-lose :get) '(RESTING::VERB RESTING-VERBS:GET)))
    (is (equal (verb-spec-or-lose "GET") '(RESTING::VERB RESTING-VERBS:GET)))
    (is (equal (verb-spec-or-lose '(v resting-verbs:get)) '(V RESTING-VERBS:GET)))))

(deftest parse-content-types ()
  (macrolet ((cts (x)
               `(handler-bind ((style-warning #'muffle-warning))
                 (content-type-spec-or-lose-1 ,x))))
    (is (equal (cts '(foo t)) '(FOO RESTING-TYPES:CONTENT)))
    (is (equal (cts '(foo "*/*")) '(FOO RESTING-TYPES:CONTENT)))
    (is (equal (cts :text/html) '(TYPE RESTING-TYPES:TEXT/HTML)))
    (is (equal (cts "text/html") '(TYPE RESTING-TYPES:TEXT/HTML)))
    (is (equal (cts '"text/html") '(TYPE RESTING-TYPES:TEXT/HTML)))
    (is (equal (cts :text/*) '(TYPE RESTING-TYPES:TEXT)))
    (is (equal (cts "text/*") '(TYPE RESTING-TYPES:TEXT)))
    (is (equal (cts 'foo) '(FOO RESTING-TYPES:CONTENT)))
    (is (equal (cts '(foo "text/html")) '(FOO RESTING-TYPES:TEXT/HTML)))

    (signals error (cts '"text/html-typo"))))

(defpackage :resting-parse-uri-tests
  (:use :resting :cl)
  (:export #:bla #:root #:yo))
(in-package :resting-parse-uri-tests)

(defresource bla (a b c &key &allow-other-keys))

(defroute bla (a b c &key foo resting:fragment &allow-other-keys)
  (declare (ignore resting:fragment foo c)))

(defresource root (verb content-type file))

(defresource yo (verb content-type))

(in-package :resting-tests)

(defun parse-uri-1 (uri acceptor)
  (let* ((match (position #\? uri))
         (script-name (if match (subseq uri 0 match) uri))
         (query-string (and match (subseq uri (1+ match)))))
    (parse-uri script-name query-string acceptor)))

(deftest test-parse-uri (&optional (acceptor
                                    (make-instance 'rest-acceptor
                                      :route-packages '(:resting-parse-uri-tests))))
  (multiple-value-bind (resource args)
      (parse-uri-1 "/bla/ble/bli?foo=fonix;bar=fotrix#coisoetal" acceptor)
    (is (equal args
               '("ble" "bli" :FOO "fonix" :BAR "fotrix" RESTING:FRAGMENT "coisoetal")))
    (is (eq resource #'resting-parse-uri-tests:bla)))
  
  (multiple-value-bind (resource args)
      (parse-uri-1 "/ignored/bla/ble/bli?foo=fonix;bar=fotrix#coisoetal"
                   (make-instance 'rest-acceptor
                     :route-packages (route-packages acceptor)
                     :resource-name-regexp "/ignored/([^/]+)/"))
    (is (equal args
               '("ble" "bli" :FOO "fonix" :BAR "fotrix" RESTING:FRAGMENT "coisoetal")))
    (is (eq resource #'resting-parse-uri-tests:bla)))
  
  (multiple-value-bind (resource args)
      (parse-uri-1 "/bla/ble/bli" acceptor)
    (is (equal args '("ble" "bli")))
    (is (eq resource #'resting-parse-uri-tests:bla)))

  ;; content-types in the extension
  ;;
  (multiple-value-bind (resource args content-type)
      (parse-uri-1 "/yo?foo=ok" acceptor)
    (is (equal args '(:foo "ok")))
    (is (eq resource #'resting-parse-uri-tests:yo))
    (is (eq content-type nil)))
  
  (multiple-value-bind (resource args content-type)
      (parse-uri-1 "/yo.css?foo=ok" acceptor)
    (is (equal args '(:foo "ok")))
    (is (eq resource #'resting-parse-uri-tests:yo))
    (is (eq content-type (find-class 'resting-types:text/css))))

  (multiple-value-bind (resource args content-type)
      (parse-uri-1 "/yo/1.css?foo=ok" acceptor)
    (is (equal args '("1" :foo "ok")))
    (is (eq resource #'resting-parse-uri-tests:yo))
    (is (eq content-type (find-class 'resting-types:text/css))))

  (multiple-value-bind (resource args content-type)
      (parse-uri-1 "/yo.unknownextension?foo=ok" acceptor)
    (is (equal args '(:foo "ok")))
    (is (eq resource #'resting-parse-uri-tests:yo))
    (is (not content-type)))

  (multiple-value-bind (resource args content-type)
      (parse-uri-1 "/yo/arg.unknownextension?foo=ok" acceptor)
    (is (equal args '("arg.unknownextension" :foo "ok")))
    (is (eq resource #'resting-parse-uri-tests:yo))
    (is (not content-type))))


;;; Some tests from the READEM.md
;;;
(defpackage :resting-demo (:use :cl))
(in-package :resting-demo)

(defparameter *todo-counter* 0)

(defclass todo ()
  ((id :initform (incf *todo-counter*) :accessor todo-id)
   (task :initarg :task :accessor todo-task)
   (done :initarg :done :accessor todo-done)))

(defparameter *todos* 
  (list (make-instance 'todo :task "Wash dishes")
        (make-instance 'todo :task "Scrub floor")
        (make-instance 'todo :task "Doze off" :done t)))

(defmethod print-object ((x todo) s)
  (print-unreadable-object (x s)
    (format s "~a \"~a\"" (todo-id x) (todo-task x))))

(resting:defroute todo (:get "text/plain" id &key maybe)
  (declare (ignore maybe))
  (let ((todo (find id *todos* :key #'todo-id)))
    (if todo
        (todo-task todo)
        (error 'resting:404 :format-control "No such todo!"))))

(resting:defroute todo (:get "text/css" id &key maybe)
  (declare (ignore maybe))
  (format nil "The CSS for TODO item ~a" id))

(resting:defroute todo (:put (content "text/plain") id &key maybe)
  (declare (ignore maybe))
  (let ((todo (find id *todos* :key #'todo-id)))
    (if todo
        (setf (todo-task todo)
              (babel:octets-to-string
               (resting:content-body content)))
        (error 'resting:404 :format-control "No such todo!"))))

(eval-when (:compile-toplevel :load-toplevel :execute)
 (resting:define-content "application/json"))

(resting:defresource todos (method content))

(resting:defroute todos (:get "text/plain")
  (format nil "~{~a~^~%~}" (mapcar #'todo-task *todos*)))

(resting:defroute todos (:get "application/json")
  (format nil "{NOTREALLYJSON~{~a~^~%~}NOTREALLYJSON}"
          (mapcar #'todo-task *todos*)))

(in-package :resting-tests)

(defvar *use-this-acceptor* nil)
(defvar *actual-port*)

(defmacro with-request ((uri &rest morekeys &key &allow-other-keys) args &body body)
  (let ((result-sym (gensym)))
    `(let* ((,result-sym
              (multiple-value-list
               (drakma:http-request
                (format nil "http://localhost:~a~a" *actual-port* ,uri)
                ,@morekeys)))
            ,@(loop for arg in args
                    for i from 0
                    when arg
                      collect `(,arg (nth ,i ,result-sym))))
       ,@body)))

(defun call-with-acceptor-setup (use-this-acceptor packages fn)
  (let* ((acceptor (or use-this-acceptor
                       (make-instance 'resting:rest-acceptor :port 0)))
         (saved-catch-errors hunchentoot:*catch-errors-p*)
         (saved-packages (resting:route-packages acceptor)))
    (unwind-protect
         (progn
           (setq hunchentoot:*catch-errors-p* t)
           (setf (resting::route-packages acceptor) packages)
           (unless (hunchentoot::acceptor-listen-socket acceptor)
             (hunchentoot:start acceptor))
           (let ((*actual-port*
                   #+(or allegro sbcl)
                   (usocket:get-local-port (hunchentoot::acceptor-listen-socket acceptor))))
             (funcall fn)))
      (setf (resting::route-packages acceptor) saved-packages)
      (setq hunchentoot:*catch-errors-p* saved-catch-errors)
      (unless use-this-acceptor
        (hunchentoot:stop acceptor)))))

(defmacro with-acceptor-setup
    ((&key use-this-acceptor
           (packages '(:resting-demo)))
     &body body)
  `(call-with-acceptor-setup ,use-this-acceptor ,packages #'(lambda () ,@body)))

(deftest test-some-routes (&optional (use-this-acceptor *use-this-acceptor*))
  (with-acceptor-setup (:use-this-acceptor use-this-acceptor
                        :packages '(:resting-demo))
    (with-request ("/todo/1") (nil code) (is (= 200 code)))
    (with-request ("/todo/10") (nil code) (is (= 404 code)))
    ;; Test keywords args
    ;; 
    (with-request ("/todo/1?maybe=bla") (nil code) (is (= 200 code)))
    (with-request ("/todo/1?nokeyword=bla") (nil code) (is (= 404 code)))
    ;; Test "Accept:" header
    ;; 
    (with-request ("/todo/1"
                   :accept "application/json") (nil code) (is (= 404 code)))
    (with-request ("/todo/1"
                   :accept "application/*") (nil code) (is (= 404 code)))
    (with-request ("/todo/1"
                   :accept "text/*") (nil code) (is (= 200 code)))
    (with-request ("/todo/1"
                   :accept "text/plain") (nil code) (is (= 200 code)))
    (with-request ("/todo/1"
                   :accept "application/json; q=0.8,text/plain; garbage") (nil code) (is (= 200 code)))
    (with-request ("/todo/1"
                   :accept "application/json;text/plain") (nil code) (is (= 404 code)))
    (with-request ("/todos"
                   :accept "application/json;text/plain") (answer code headers)
      (is (= 200 code))
      (is (not (stringp answer)))
      (is (cl-ppcre:scan "NOTREALLYJSON" (babel:octets-to-string answer)))
      (is (string= (cdr (find :content-type headers :key #'car))
                   "APPLICATION/JSON")))
    (with-request ("/todos"
                   :accept "application/*;text/plain") (answer code headers)
      (is (= 200 code))
      (is (not (stringp answer)))
      (is (cl-ppcre:scan "NOTREALLYJSON" (babel:octets-to-string answer)))
      (is (string= (cdr (find :content-type headers :key #'car))
                   "APPLICATION/JSON")))
    (with-request ("/todos"
                   :accept "text/plain;application/json") (answer code)
      (is (= 200 code))
      (is (stringp answer)))
    (let ((random (symbol-name (gensym))))
      (with-request ("/todo/3"
                     :method :put
                     :content random
                     :content-type "text/plain") (nil code)
        (is (= 200 code)))
      (with-request ("/todo/3"
                     :method :get
                     :accept "text/plain") (answer code)
        (is (= 200 code))
        (is (string= answer random))))
    ;; content-type in extension
    ;; 
    (with-request ("/todo/1.css") (answer code)
      (is (= 200 code))
      (is (search "CSS for TODO item 1" answer)))))

(defpackage :resting-demo-customization
  (:use :cl :resting))
(in-package :resting-demo-customization)

(defresource root (verb content-type file))





