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


(deftest relations ()
  (is (subtypep 'resting-types:send-any-text 'resting-types:text/html))
  (is (subtypep 'resting-types:send-anything 'resting-types:text/html))
  (is (not (subtypep 'resting-types:send-any-text 'resting-types:application/xml))))

(defroute bla (a b c &key foo resting:fragment &allow-other-keys)
  (declare (ignore resting:fragment foo c)))

(deftest test-parse-uri ()
  (multiple-value-bind (route args)
      (parse-uri "/bla/ble/bli?foo=fonix;bar=fotrix#coisoetal"
                 (make-instance 'rest-acceptor))
    (is (equal args
               '("ble" "bli" :FOO "fonix" :BAR "fotrix" RESTING:FRAGMENT "coisoetal")))
    (is (eq 'bla route)))
  (multiple-value-bind (route args)
      (parse-uri "/ignored/bla/ble/bli?foo=fonix;bar=fotrix#coisoetal"
                 (make-instance 'rest-acceptor
                   :resource-name-regexp "/ignored/([^/]+)/"))
    (is (equal args
               '("ble" "bli" :FOO "fonix" :BAR "fotrix" RESTING:FRAGMENT "coisoetal")))
    (is (eq 'bla route)))
  (multiple-value-bind (route args)
      (parse-uri "/bla/ble/bli" (make-instance 'rest-acceptor))
    (is (equal args '("ble" "bli")))
    (is (eq 'bla route))))


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

(resting:defroute todos (:get "text/plain")
  (format nil "~{~a~^~%~}" (mapcar #'todo-task *todos*)))

(in-package :resting-tests)

(deftest test-some-routes ()
  (let ((acceptor (make-instance 'resting:rest-acceptor :port 0
                                 :route-packages (list (find-package :resting-demo))))
        (saved-catch-errors hunchentoot:*catch-errors-p*))
    (unwind-protect
         (macrolet ((with-request (uri args &body body)
                        (let ((result-sym (gensym)))
                          `(let* ((,result-sym
                                    (multiple-value-list
                                     (drakma:http-request (format nil "http://localhost:~a~a" actual-port ,uri))))
                                  ,@(loop for arg in args
                                          for i from 0
                                          when arg
                                            collect `(,arg (nth ,i ,result-sym))))
                             ,@body))))
           (setq hunchentoot:*catch-errors-p* t)
           (hunchentoot:start acceptor)
           (let ((actual-port
                   #+allegro
                   (usocket:get-local-port (hunchentoot::acceptor-listen-socket acceptor))
                   #-allegro
                   (error "sorry, this test doesn't work")))
             (with-request "/todo/1" (nil code) (is (= 200 code)))
             (with-request "/todo/10" (nil code) (is (= 404 code)))
             (with-request "/todo/1?maybe=bla" (nil code) (is (= 200 code)))
             (with-request "/todo/1?nokeyword=bla" (nil code) (is (= 404 code)))))
      (setq hunchentoot:*catch-errors-p* saved-catch-errors)
      (hunchentoot:stop acceptor))))


