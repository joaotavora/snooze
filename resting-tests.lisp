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
  (declare (ignore resting:fragment foo a b c)))

(deftest test-parse-uri ()
  (is (equal (parse-args-in-uri "/bla/ble/bli?foo=fonix;bar=fotrix#coisoetal")
             '("bla" "ble" "bli" :FOO "fonix" :BAR "fotrix" RESTING:FRAGMENT "coisoetal")))
  (is (eq 'bla
          (parse-uri "frob/bla/ble/bli?foo=fonix;bar=fotrix#coisoetal"
                     (make-instance 'rest-acceptor)))))


