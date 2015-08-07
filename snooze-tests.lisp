(fiasco:define-test-package :snooze-tests
  (:use #:cl #:snooze)
  (:import-from  #:snooze-common
                 #:verb-spec-or-lose
                 #:find-content-class
                 #:content-type-spec-or-lose-1
                 #:parse-resource))
(in-package :snooze-tests)

(deftest parse-verbs ()
  (handler-bind ((style-warning #'muffle-warning))
    (is (equal (verb-spec-or-lose '(foo t)) '(FOO SNOOZE-VERBS:HTTP-VERB)))
    (is (equal (verb-spec-or-lose :get)     '(SNOOZE-VERBS:HTTP-VERB SNOOZE-VERBS:GET)))
    (is (equal (verb-spec-or-lose "GET")    '(SNOOZE-VERBS:HTTP-VERB SNOOZE-VERBS:GET)))
    (is (equal (verb-spec-or-lose '(v snooze-verbs:get)) '(V SNOOZE-VERBS:GET)))))

(deftest parse-content-types ()
  (macrolet ((cts (x)
               `(handler-bind ((style-warning #'muffle-warning))
                  (content-type-spec-or-lose-1 ,x))))
    (is (equal (cts '(foo t)) '(FOO SNOOZE-TYPES:CONTENT)))
    (is (equal (cts '(foo "*/*")) '(FOO SNOOZE-TYPES:CONTENT)))
    (is (equal (cts :text/html) '(TYPE SNOOZE-TYPES:TEXT/HTML)))
    (is (equal (cts "text/html") '(TYPE SNOOZE-TYPES:TEXT/HTML)))
    (is (equal (cts '"text/html") '(TYPE SNOOZE-TYPES:TEXT/HTML)))
    (is (equal (cts :text/*) '(TYPE SNOOZE-TYPES:TEXT)))
    (is (equal (cts "text/*") '(TYPE SNOOZE-TYPES:TEXT)))
    (is (equal (cts 'foo) '(FOO SNOOZE-TYPES:CONTENT)))
    (is (equal (cts '(foo "text/html")) '(FOO SNOOZE-TYPES:TEXT/HTML)))

    (signals error (cts '"text/html-typo"))))

(cl:defpackage #:snooze-parse-uri-tests
  (:use :snooze :cl)
  (:export #:bla #:root #:yo))
(in-package :snooze-parse-uri-tests)

(defresource bla (a b c &key &allow-other-keys))

(defroute bla (a b c &key foo snooze:fragment &allow-other-keys)
  (declare (ignore snooze:fragment foo c)))

(defresource root (verb content-type file))

(defresource yo (verb content-type))

(defresource yo.snooze (verb content-type))

(in-package :snooze-tests)

(deftest test-parse-uri ()
  (multiple-value-bind (resource pargs kwargs)
      (parse-resource "/bla/ble/bli?foo=fonix;bar=fotrix#coisoetal")
    (is (equal pargs '("ble" "bli")))
    (is (equal kwargs '(:FOO "fonix" :BAR "fotrix" SNOOZE:FRAGMENT "coisoetal")))
    (is (eq resource #'snooze-parse-uri-tests:bla)))
  
  (multiple-value-bind (resource pargs kwargs)
      (let ((snooze:*resource-name-function*
              (lambda (ignored resource &rest args) (declare (ignore ignored)) (values resource args))))
        (parse-resource "/ignored/bla/ble/bli?foo=fonix;bar=fotrix#coisoetal"))
    (is (equal pargs '("ble" "bli")))
    (is (equal kwargs '(:FOO "fonix" :BAR "fotrix" SNOOZE:FRAGMENT "coisoetal")))
    (is (eq resource #'snooze-parse-uri-tests:bla)))
  
  (multiple-value-bind (resource pargs)
      (parse-resource "/bla/ble/bli")
    (is (equal pargs '("ble" "bli")))
    (is (eq resource #'snooze-parse-uri-tests:bla)))

  ;; content-types in the extension
  ;;
  (multiple-value-bind (resource pargs kwargs content-types)
      (parse-resource "/yo?foo=ok")
    (declare (ignore pargs))
    (is (equal kwargs '(:foo "ok")))
    (is (eq resource #'snooze-parse-uri-tests:yo))
    (is (null content-types)))
  
  (multiple-value-bind (resource pargs kwargs content-types)
      (parse-resource "/yo.css?foo=ok")
    (declare (ignore pargs))
    (is (equal kwargs '(:foo "ok")))
    (is (eq resource #'snooze-parse-uri-tests:yo))
    (is (member (find-class 'snooze-types:text/css) content-types)))

  (multiple-value-bind (resource pargs kwargs content-types)
      (parse-resource "/yo/1.css?foo=ok")
    (is (equal pargs '("1")))
    (is (equal kwargs '(:foo "ok")))
    (is (eq resource #'snooze-parse-uri-tests:yo))
    (member (find-class 'snooze-types:text/css) content-types))

  (multiple-value-bind (resource pargs kwargs content-types)
      (let ((snooze:*uri-content-types-function* nil))
        (parse-resource "/yo/1.css?foo=ok"))
    (is (equal pargs '("1.css")))
    (is (equal kwargs '(:foo "ok")))
    (is (eq resource #'snooze-parse-uri-tests:yo))
    (member (find-class 'snooze-types:text/css) content-types))

  (multiple-value-bind (resource pargs kwargs content-types)
      (parse-resource "/yo.snooze?foo=ok")
    (declare (ignore pargs))
    (is (equal kwargs '(:foo "ok")))
    (is (eq resource #'snooze-parse-uri-tests::yo.snooze))
    (is (null content-types)))

  (multiple-value-bind (resource pargs kwargs content-type)
      (parse-resource "/yo/arg.unknownextension?foo=ok")
    (is (equal pargs '("arg.unknownextension")))
    (is (equal kwargs '(:foo "ok")))
    (is (eq resource #'snooze-parse-uri-tests:yo))
    (is (not content-type))))


;;; Some tests from the README.md
;;;
(defvar *mock-http-payload* "fornix")

(cl:defpackage :snooze-tests-demo (:use :cl :snooze))
(in-package :snooze-tests-demo)

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

(snooze:defroute todo (:get "text/plain" id &key maybe)
  (declare (ignore maybe))
  (let ((todo (find id *todos* :key #'todo-id)))
    (if todo
        (todo-task todo)
        (http-condition 404 "No such TODO!"))))

(snooze:defroute todo (:get "text/css" id &key maybe)
  (declare (ignore maybe))
  (format nil "The CSS for TODO item ~a" id))

(snooze:defroute todo (:put (content "text/plain") id &key maybe)
  (declare (ignore maybe))
  (let ((todo (find id *todos* :key #'todo-id)))
    (if todo
        (setf (todo-task todo)
              snooze-tests::*mock-http-payload*)
        (http-condition 404 "No such TODO!"))))

(defmethod todo ((snooze-verbs:http-verb snooze-verbs:put)
                        (content snooze-types:text/plain) id &key
                        maybe)
         (declare (ignore maybe))
         (let ((todo (find id *todos* :key #'todo-id)))
           (if todo
               (setf (todo-task todo) snooze-tests::*mock-http-payload*)
             (http-condition 404 "No such TODO!"))))

(snooze:defresource todos (method content))

(snooze:defroute todos (:get "text/plain")
  (format nil "~{~a~^~%~}" (mapcar #'todo-task *todos*)))

(snooze:defroute todos (:get "application/json")
  (format nil "{NOTREALLYJSON~{~a~^~%~}NOTREALLYJSON}"
          (mapcar #'todo-task *todos*)))

(defmethod snooze:explain-condition ((c http-condition) (resource (eql #'todo)) (ct snooze-types:application/json))
  "{Oooops-in-JSON}")

(in-package :snooze-tests)

(defmacro with-request ((uri &rest morekeys &key &allow-other-keys) args &body body)
  (let ((result-sym (gensym)))
    `(let* ((,result-sym
              (multiple-value-list
               (snooze:handle-request
                ,uri
                ,@morekeys)))
            ,@(loop for arg in args
                    for i from 0
                    when arg
                      collect `(,arg (nth ,i ,result-sym))))
       ,@body)))

(deftest test-some-routes ()
  (with-request ("/todo/1") (code) (is (= 200 code)))
  (with-request ("/todo/10") (code) (is (= 404 code)))
  ;; Test keywords args
  ;; 
  (with-request ("/todo/1?maybe=bla") (code) (is (= 200 code)))
  (with-request ("/todo/1?nokeyword=bla") (code) (is (= 400 code)))
  ;; Test "Accept:" header
  ;; 
  (with-request ("/todo/1"
                 :accept "application/json") (code) (is (= 406 code)))
  (with-request ("/todo/1"
                 :accept "application/*") (code) (is (= 406 code)))
  (with-request ("/todo/1"
                 :accept "text/*") (code) (is (= 200 code)))
  (with-request ("/todo/1"
                 :accept "text/plain") (code) (is (= 200 code)))
  (with-request ("/todo/1"
                 :accept "application/json; q=0.8,text/plain; garbage") (code) (is (= 200 code)))
  (with-request ("/todo/1"
                 :accept "application/json;text/plain") (code) (is (= 406 code)))
  (with-request ("/todos" 
                 :accept "application/json;text/plain") (code payload ct)
    (is (= 200 code))
    (is (cl-ppcre:scan "NOTREALLYJSON" payload))
    (is (equal ct "application/json")))
  (with-request ("/todos"
                 :accept "application/*;text/plain") (code payload ct)
    (is (= 200 code))
    (is (cl-ppcre:scan "NOTREALLYJSON" payload))
    (is (equal ct "application/json")))
  (with-request ("/todos"
                 :accept "text/plain;application/json") (code payload ct)
    (is (= 200 code))
    (is (stringp payload))
    (is (equal ct "text/plain")))
  (let ((*mock-http-payload* (symbol-name (gensym))))
    (with-request ("/todo/3"
                   :method :put
                   :content-type "text/plain") (code)
      (is (= 200 code)))
    (with-request ("/todo/3"
                   :method :get
                   :accept "text/plain") (code payload)
      (is (= 200 code))
      (is (string= payload *mock-http-payload*))))
  ;; content-type in extension
  ;; 
  (with-request ("/todo/1.css") (code payload)
    (is (= 200 code))
    (is (search "CSS for TODO item 1" payload))))


;;; Genpath section
;;; 
(defresource book-resource (verb content-type file user &optional (coiso "coiso") (tal "bla") &key fornix (yo "yobla"))
  (:genpath book-resource-path)
  (:route :around (:get "text/plain" file user &optional (coiso "coiso") (tal "bla") &key fornix (yo "yobla"))
          (declare (ignore file user coiso tal fornix yo))))

(deftest path-generation ()
  (is (string= (book-resource-path "yo" "yeah" nil nil)
               "/book-resource/yo/yeah?yo=yobla"))
  (signals error (book-resource-path "yo" "yeah" nil "AHA"))
  (is (string= (book-resource-path "yo" "yeah" "OK" nil)
               "/book-resource/yo/yeah/OK?yo=yobla"))
  (is (string= (book-resource-path "yo" "yeah" "OK" nil :yo "mama" :fornix nil)
               "/book-resource/yo/yeah/OK?yo=mama"))
  (is (string= (book-resource-path "yo" "yeah") "/book-resource/yo/yeah/coiso/bla?yo=yobla"))
  (is (string= (book-resource-path "yo with a space" "yeah") "/book-resource/yo with a space/yeah/coiso/bla?yo=yobla")))




