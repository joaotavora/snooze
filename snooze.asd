(asdf:defsystem :snooze
  :depends-on (#:alexandria #:hunchentoot #:cl-ppcre #:closer-mop)
  :serial t
  :components ((:file "package")
               (:file "snooze")))

(asdf:defsystem :snooze-tests
  :depends-on (#:snooze #:fiasco #:drakma #:babel)
  :serial t
  :components ((:file "snooze-tests")))
