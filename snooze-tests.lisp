(fiasco:define-test-package :snooze-tests
  (:use #:cl #:snooze)
  (:import-from  #:snooze-common
                 #:verb-spec-or-lose
                 #:content-type-spec-or-lose-1)
  (:import-from #:snooze-utils
                #:parse-uri))
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

(in-package :snooze-tests)

(defun parse-uri-1 (uri server)
  (let* ((match (position #\? uri))
         (script-name (if match (subseq uri 0 match) uri))
         (query-string (and match (subseq uri (1+ match)))))
    (parse-uri script-name query-string server)))

(deftest test-parse-uri (&optional (server
                                    (make-instance 'snooze-server
                                                   :route-packages '(:snooze-parse-uri-tests))))
  (multiple-value-bind (resource args)
      (parse-uri-1 "/bla/ble/bli?foo=fonix;bar=fotrix#coisoetal" server)
    (is (equal args
               '("ble" "bli" :FOO "fonix" :BAR "fotrix" SNOOZE:FRAGMENT "coisoetal")))
    (is (eq resource #'snooze-parse-uri-tests:bla)))
  
  (multiple-value-bind (resource args)
      (parse-uri-1 "/ignored/bla/ble/bli?foo=fonix;bar=fotrix#coisoetal"
                   (make-instance 'snooze-server
                                  
                                  :route-packages (route-packages server)
                                  :resource-name-regexp "/ignored/([^/]+)/"))
    (is (equal args
               '("ble" "bli" :FOO "fonix" :BAR "fotrix" SNOOZE:FRAGMENT "coisoetal")))
    (is (eq resource #'snooze-parse-uri-tests:bla)))
  
  (multiple-value-bind (resource args)
      (parse-uri-1 "/bla/ble/bli" server)
    (is (equal args '("ble" "bli")))
    (is (eq resource #'snooze-parse-uri-tests:bla)))

  ;; content-types in the extension
  ;;
  (multiple-value-bind (resource args content-type)
      (parse-uri-1 "/yo?foo=ok" server)
    (is (equal args '(:foo "ok")))
    (is (eq resource #'snooze-parse-uri-tests:yo))
    (is (eq content-type nil)))
  
  (multiple-value-bind (resource args content-type)
      (parse-uri-1 "/yo.css?foo=ok" server)
    (is (equal args '(:foo "ok")))
    (is (eq resource #'snooze-parse-uri-tests:yo))
    (is (eq content-type (find-class 'snooze-types:text/css))))

  (multiple-value-bind (resource args content-type)
      (parse-uri-1 "/yo/1.css?foo=ok" server)
    (is (equal args '("1" :foo "ok")))
    (is (eq resource #'snooze-parse-uri-tests:yo))
    (is (eq content-type (find-class 'snooze-types:text/css))))

  (multiple-value-bind (resource args content-type)
      (parse-uri-1 "/yo.unknownextension?foo=ok" server)
    (is (equal args '(:foo "ok")))
    (is (eq resource #'snooze-parse-uri-tests:yo))
    (is (not content-type)))

  (multiple-value-bind (resource args content-type)
      (parse-uri-1 "/yo/arg.unknownextension?foo=ok" server)
    (is (equal args '("arg.unknownextension" :foo "ok")))
    (is (eq resource #'snooze-parse-uri-tests:yo))
    (is (not content-type))))


;;; Some tests from the READEM.md
;;;
(cl:defpackage :snooze-demo (:use :cl))
(in-package :snooze-demo)

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
        (error 'snooze:404 :format-control "No such todo!"))))

(snooze:defroute todo (:get "text/css" id &key maybe)
  (declare (ignore maybe))
  (format nil "The CSS for TODO item ~a" id))

(snooze:defroute todo (:put (content "text/plain") id &key maybe)
  (declare (ignore maybe))
  (let ((todo (find id *todos* :key #'todo-id)))
    (if todo
        (setf (todo-task todo)
              (babel:octets-to-string
               (snooze:request-body)))
        (error 'snooze:404 :format-control "No such todo!"))))

(snooze:defresource todos (method content))

(snooze:defroute todos (:get "text/plain")
  (format nil "~{~a~^~%~}" (mapcar #'todo-task *todos*)))

(snooze:defroute todos (:get "application/json")
  (format nil "{NOTREALLYJSON~{~a~^~%~}NOTREALLYJSON}"
          (mapcar #'todo-task *todos*)))

(in-package :snooze-tests)

(defvar *use-this-server* nil)
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

(defun call-with-server-setup (use-this-server packages fn)
  (let* ((server (or use-this-server
                     (make-instance 'snooze:snooze-server :port 3434
                                    :route-packages nil
                                    :backend :clack)))
         (saved-catch-errors hunchentoot:*catch-errors-p*)
         (saved-packages (snooze:route-packages server)))
    (unwind-protect
         (progn
           (setq hunchentoot:*catch-errors-p* t)
           (setf (snooze::route-packages server) packages)
           (snooze:start server)
           (let ((*actual-port*
                   3434
                   ;; (clacktest::server-port (snooze::backend server))
                   ))
             (funcall fn)))
      (setf (snooze::route-packages server) saved-packages)
      (setq hunchentoot:*catch-errors-p* saved-catch-errors)
      (unless use-this-server
        (snooze:stop server)))))

(defmacro with-server-setup
    ((&key use-this-server
           (packages ''(:snooze-demo)))
     &body body)
  `(call-with-server-setup ,use-this-server ,packages #'(lambda () ,@body)))

(deftest test-some-routes (&optional (use-this-server *use-this-server*))
  (with-server-setup (:use-this-server use-this-server
                      :packages '(:snooze-demo))
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


;;; Genurl section
;;; 
(fiasco:define-test-package :snooze-demo-fancy
  (:use :cl :snooze))
(in-package :snooze-demo-fancy)

(defresource book (verb content-type file user &optional (coiso "coiso") (tal "bla") &key fornix (yo "yobla"))
  (:genurl book-url)
  (:route :around (:get "text/plain" file user &optional (coiso "coiso") (tal "bla") &key fornix (yo "yobla"))
          (declare (ignore file user coiso e tal fornix yo))))

(defresource papyrus (verb content-type file user &key protocol host)
  (:genurl papyrus-url))

(defresource testament (verb content-type &optional a &rest anything)
  (:genurl testament-url))

(deftest genurl-madness ()
  (is (string= (book-url "yo" "yeah" nil nil :protocol "bla" :host "ble")
               "bla://ble/book/yo/yeah?yo=yobla"))
  (signals error (book-url "yo" "yeah" nil "AHA" :protocol "bla" :host "ble"))
  (is (string= (book-url "yo" "yeah" "OK" nil :protocol "bla" :host "ble")
               "bla://ble/book/yo/yeah/OK?yo=yobla"))
  (is (string= (book-url "yo" "yeah" "OK" nil :protocol "bla" :host "ble" :yo "mama" :fornix nil)
               "bla://ble/book/yo/yeah/OK?yo=mama"))
  (is (string= (book-url "yo" "yeah") "book/yo/yeah/coiso/bla?yo=yobla"))
  
  ;; This one remembered to have keyword args named "protocol" and "host"
  ;;
  (is (string= (papyrus-url "a" "b" :protocol "shit"
                            'snooze-syms:protocol "https" 'snooze-syms:host "localhost")
               "https://localhost/papyrus/a/b?protocol=shit"))
  (is (string= (papyrus-url "a" "b" :protocol "shit"
                                    :protocol "https" 'snooze-syms:host "localhost")
               "http://localhost/papyrus/a/b?protocol=shit"))
  (is (string= (papyrus-url "a" "b" :protocol "https" 'snooze-syms:host "localhost")
               "http://localhost/papyrus/a/b?protocol=https"))
  (signals error (papyrus-url "a" "b" :protocol "https" 'snooze-syms:protocol :ssh)))




