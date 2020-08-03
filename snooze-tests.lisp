(cl:defpackage :snooze-tests-demo
  (:use #:cl #:snooze)
  (:export #:todo-id
           #:todo-task
           #:todo
           #:*todos*
           #:*mock-http-payload*))

(fiasco:define-test-package :snooze-tests
  (:use #:cl #:snooze)
  (:import-from  #:snooze-common
                 #:verb-spec-or-lose
                 #:find-content-class
                 #:content-type-spec-or-lose-1
                 #:parse-resource)
  (:import-from #:snooze-tests-demo
                #:todo-id
                #:todo-task
                #:todo
                #:*todos*
                #:*mock-http-payload*))
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
    (is (equal (cts :text/html) '(SNOOZE-TYPES:TYPE SNOOZE-TYPES:TEXT/HTML)))
    (is (equal (cts "text/html") '(SNOOZE-TYPES:TYPE SNOOZE-TYPES:TEXT/HTML)))
    (is (equal (cts '"text/html") '(SNOOZE-TYPES:TYPE SNOOZE-TYPES:TEXT/HTML)))
    (is (equal (cts :text/*) '(SNOOZE-TYPES:TYPE SNOOZE-TYPES:TEXT)))
    (is (equal (cts "text/*") '(SNOOZE-TYPES:TYPE SNOOZE-TYPES:TEXT)))
    (is (equal (cts 'foo) '(FOO SNOOZE-TYPES:CONTENT)))
    (is (equal (cts '(foo "text/html")) '(FOO SNOOZE-TYPES:TEXT/HTML)))

    (signals error (cts '"text/html-typo"))))

(defresource bla (a b c &key &allow-other-keys))

(defroute bla (a b c &key foo snooze:fragment &allow-other-keys)
  (declare (ignore snooze:fragment foo c)))

(defresource root (verb content-type file))

(defresource yo (verb content-type))

(defresource yo.snooze (verb content-type))

(defun parse-uri-for-tests (uri)
  (multiple-value-bind (resource content-types moreuri)
      (parse-resource uri)
    (multiple-value-bind (plain-args keyword-args)
        (uri-to-arguments resource moreuri)
      (values resource
              plain-args
              keyword-args
              content-types))))

(deftest test-parse-uri ()
  ;; HACK: Interning BAR here, in :SNOOZE-TESTS is needed, because,
  ;; even with &ALLOW-OTHER-KEYS, snooze refuses, by default, to
  ;; create keyword symbols that don't have a corresponding symbol in
  ;; the resource's package.
  ;;
  (intern "BAR" (snooze::resource-package
                (snooze::find-resource 'bla)))
  (multiple-value-bind (resource pargs kwargs)
      (parse-uri-for-tests "/bla/ble/bli?foo=fonix;bar=fotrix#coisoetal")
    (declare (ignore pargs kwargs))
    (is (eq resource #'bla)))

  (multiple-value-bind (resource pargs kwargs)
      (let ((snooze:*resource-name-function*
              (lambda (uri)
                (default-resource-name (subseq uri (mismatch "/ignored" uri))))))
        (parse-uri-for-tests "/ignored/bla/ble/bli?foo=fonix;bar=fotrix#coisoetal"))
    (is (equal pargs '(ble bli)))
    (is (equal kwargs '((:FOO            . fonix)
                        (:BAR            . fotrix)
                        (SNOOZE:FRAGMENT . coisoetal))))
    (is (eq resource #'bla)))

  (multiple-value-bind (resource pargs)
      (parse-uri-for-tests "/bla/ble/bli")
    (is (equal pargs '(ble bli)))
    (is (eq resource #'bla)))

  ;; content-types in the extension
  ;;
  (multiple-value-bind (resource pargs kwargs content-types)
      (parse-uri-for-tests "/yo?foo=ok")
    (declare (ignore pargs))
    (is (equal kwargs '((:foo . ok))))
    (is (eq resource #'yo))
    (is (null content-types)))

  (multiple-value-bind (resource pargs kwargs content-types)
      (parse-uri-for-tests "/yo.css?foo=ok")
    (is (null pargs))
    (is (equal kwargs '((:foo . ok))))
    (is (eq resource #'yo))
    (is (member (find-class 'snooze-types:text/css) content-types)))

  (multiple-value-bind (resource pargs kwargs content-types)
      (parse-uri-for-tests "/yo/1.css?foo=ok")
    (is (equal pargs '(1)))
    (is (equal kwargs '((:foo . ok))))
    (is (eq resource #'yo))
    (member (find-class 'snooze-types:text/css) content-types))

  (multiple-value-bind (resource pargs kwargs content-types)
      (let ((snooze:*uri-content-types-function* nil))
        (parse-uri-for-tests "/yo/1.css?foo=ok"))
    (is (equal pargs '(1.css)))
    (is (equal kwargs '((:foo . ok))))
    (is (eq resource #'yo))
    (member (find-class 'snooze-types:text/css) content-types))

  (multiple-value-bind (resource pargs kwargs content-types)
      (parse-uri-for-tests "/yo.snooze?foo=ok")
    (is (null pargs))
    (is (equal kwargs '((:foo . ok))))
    (is (eq resource #'yo.snooze))
    (is (null content-types)))

  (multiple-value-bind (resource pargs kwargs content-type)
      (parse-uri-for-tests "/yo/arg.unknownextension?foo=ok")
    (is (equal pargs '(arg.unknownextension)))
    (is (equal kwargs '((:foo . ok))))
    (is (eq resource #'yo))
    (is (not content-type))))


;;; Some tests from the README.md
;;;
(in-package :snooze-tests-demo)

(defvar *mock-http-payload* "fornix")

(defparameter *todo-counter* 0)

(defclass todo ()
  ((id :initform (incf *todo-counter*) :initarg :id :accessor todo-id)
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
              *mock-http-payload*)
        (http-condition 404 "No such TODO!"))))

(defmethod todo ((snooze-verbs:http-verb snooze-verbs:put)
                        (content snooze-types:text/plain) id &key
                        maybe)
         (declare (ignore maybe))
         (let ((todo (find id *todos* :key #'todo-id)))
           (if todo
               (setf (todo-task todo) *mock-http-payload*)
             (http-condition 404 "No such TODO!"))))

(snooze:defresource todos (method content))

(snooze:defroute todos (:get "text/plain")
  (format nil "~{~a~^~%~}" (mapcar #'todo-task *todos*)))

(snooze:defroute todos (:get "application/json")
  (format nil "{NOTREALLYJSON~{~a~^~%~}NOTREALLYJSON}"
          (mapcar #'todo-task *todos*)))

(defmethod snooze:explain-condition ((c http-condition) (resource (eql #'todo))
                                     (ct snooze-types:application/json))
  "{Oooops-in-JSON}")

(in-package :snooze-tests)

(defmacro with-request ((uri &rest morekeys &key &allow-other-keys) args &body body)
  (let ((result-sym (gensym)))
    `(let* ((snooze:*catch-errors* nil)
            (snooze:*catch-http-conditions* t)
            (,result-sym
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
  (with-request ("/todo/1?maybe=an-unknown-symbol") (code) (is (= 200 code)))
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
                 :accept "application/json; q=0.8,text/plain; garbage") (code)
    (is (= 200 code)))
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

(snooze:defroute todo (:delete ignored-type id &key &allow-other-keys)
  (let ((todo (find id *todos* :key #'todo-id)))
    (cond (todo
           (setf *todos* (delete todo *todos*))
           (todo-task todo))
          (t
           (http-condition 404 "No such TODO!")))))

(deftest test-delete-route ()
  (let ((*todos*
          (list (make-instance 'todo
                               :id 1 :done t :task "Wash")
                (make-instance 'todo
                               :id 3 :done t :task "Clean"))))
    (with-request ("/todo/3"
                   :method :delete
                   :content-type "text/irrelevant") (code)
      (is (= 200 code))
      (is (null (find 3 *todos* :key #'todo-id))))
    (with-request ("/todo/3"
                   :method :delete
                   :content-type "text/irrelevant") (code)
      (is (= 404 code)))))


;;; Genpath section
;;;
(defresource book-resource (verb content-type file user &optional
                                 (coiso 'genpath-default-coiso)
                                 (tal 'genpath-default-tal)
                                 &key fornix (yo 'genpath-default-yobla))
  (:genpath book-resource-path)
  (:route (:get "text/plain" file user &optional (coiso 'default-coiso)
                (tal 'default-tal) &key fornix (yo 'yobla))
          (write-to-string (list file user coiso tal fornix yo))))

(defresource manuscript-resource (verb content-type file user &key)
  (:genpath manuscript-resource-path)
  (:route (:get "text/plain" file user &key fornix (yo 'default-yo))
          (write-to-string (list file user fornix yo))))

(deftest path-generation ()
  (is (string= (book-resource-path 'yo 'yeah nil nil)
               "/book-resource/yo/yeah?yo=genpath-default-yobla"))
  (signals error (book-resource-path "yo" "yeah" nil "AHA"))
  (is (string= (book-resource-path 'yo 'yeah "MixedCase" nil)
               "/book-resource/yo/yeah/%22MixedCase%22?yo=genpath-default-yobla"))
  (is (string= (book-resource-path 'yo 'yeah "OK" nil :yo 'mama :fornix nil)
               "/book-resource/yo/yeah/%22OK%22?yo=mama"))
  (is (string= (book-resource-path 'yo 'yeah)
               "/book-resource/yo/yeah/genpath-default-coiso/genpath-default-tal?yo=genpath-default-yobla"))
  (is (string= (book-resource-path "yo with a space" 'yeah)
               "/book-resource/%22yo%20with%20a%20space%22/yeah/genpath-default-coiso/genpath-default-tal?yo=genpath-default-yobla")))

(defresource joaot (verb ct one two three &key quatro)
  (:genpath joaot-path))

(deftest path-parse-back ()
  (with-request ((book-resource-path 'yo 'yeah)) (code payload)
    (is (= 200 code))
    (is (equal (read-from-string payload)
               '(yo yeah genpath-default-coiso genpath-default-tal nil
                 genpath-default-yobla))))
  (with-request ((manuscript-resource-path 'yo 'yeah)) (code payload)
    (is (= 200 code))
    (is (equal (read-from-string payload)
               '(yo yeah nil default-yo))))
  (with-request ((manuscript-resource-path 'yo 'read-char)) (code payload)
    (is (= 200 code))
    (is (equal (read-from-string payload)
               '(yo cl:read-char nil default-yo))))
  (let ((uri "/joaot/1/2/3?quatro=4"))
    (is (string= (multiple-value-bind (resource content-types relative-uri)
                     (parse-resource uri)
                   (declare (ignore content-types))
                   (multiple-value-bind (plain-args keyword-args)
                       (uri-to-arguments resource relative-uri)
                     (arguments-to-uri resource plain-args keyword-args)))
                 uri))))


;;; SAFE-SIMPLE-READ tests
;;;
(deftest safe-simple-read-back ()
  (let ((crazy-package (or
                        (find-package "oh oh oh")
                        (make-package "oh oh oh"))))
    (unwind-protect
         (loop for thing in
               `(someinternalsymbol
                 "somestring"
                 123.6
                 :bla
                 123
                 0
                 ""
                 ,(intern "this" crazy-package))
               do
                  (is
                   (equal (snooze-safe-simple-read:safe-simple-read-from-string
                           (write-to-string thing))
                          thing)))
      (delete-package crazy-package))))

(deftest safe-simple-read-should-error ()
  (let ((crazy-package (or
                        (find-package "oh oh oh")
                        (make-package "oh oh oh"))))
    (unwind-protect
         (loop
           for string in
           `(,(write-to-string '(some list))
             "'quoted"
             "somestring::"
             "::"
             "something:asd:asd"
             "CL::::DEFUN")
           do
              (signals reader-error
                (snooze-safe-simple-read:safe-simple-read-from-string string)))
      (delete-package crazy-package))))
