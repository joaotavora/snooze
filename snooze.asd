(asdf:defsystem :snooze
  :depends-on (#:alexandria #:cl-ppcre #:closer-mop #:puri)
  :author "capitaomorte <https://github.com/capitaomorte>"
  :license "GPL"
  :description "Snooze is a web framework setting up REST routes using CLOS."
  :serial t
  :components ((:file "package")
               (:file "mime-types")
               (:file "common")
               (:file "api")))

(asdf:defsystem :snooze-tests
  :depends-on (#:snooze #:fiasco)
  :serial t
  :components ((:file "snooze-tests")))

(asdf:defsystem :snooze-demo
  :depends-on (#:snooze #:alexandria #:cl-who #:cl-css #:hunchentoot)
  :serial t
  :components ((:file "demo")))


