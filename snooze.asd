(asdf:defsystem :snooze
  :depends-on (#:alexandria #:hunchentoot #:cl-ppcre #:closer-mop)
  :author "capitaomorte <https://github.com/capitaomorte>"
  :license "BSD 2-clause"
  :description "Snooze is a framework for setting up routes and links to them. It invites you to use plain old CLOS generic functions to define routes. REST is assumed."
  :serial t
  :components ((:file "package")
               (:file "snooze")))

(asdf:defsystem :snooze-tests
  :depends-on (#:snooze #:fiasco #:drakma #:babel)
  :serial t
  :components ((:file "snooze-tests")))
