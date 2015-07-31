(asdf:defsystem :snooze
  :depends-on (#:alexandria #:hunchentoot #:cl-ppcre #:closer-mop)
  :author "capitaomorte <https://github.com/capitaomorte>"
  :license "GPL"
  :description "Snooze is a web framework setting up REST routes using CLOS."
  :serial t
  :components ((:file "package")
               (:file "mime-types")
               (:file "common")
               (:file "api")))

(asdf:defsystem :snooze-tests
  :depends-on (#:snooze #:fiasco #:drakma #:babel)
  :serial t
  :components ((:file "snooze-tests")))
