(asdf:defsystem :resting
  :depends-on (#:alexandria #:hunchentoot #:cl-ppcre #:closer-mop)
  :serial t
  :components ((:file "package")
               (:file "resting")))

(asdf:defsystem :resting-tests
  :depends-on (#:resting #:fiasco #:drakma #:babel)
  :serial t
  :components ((:file "resting-tests")))
