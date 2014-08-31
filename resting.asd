(asdf:defsystem :resting
  :depends-on (#:alexandria #:hunchentoot #:cl-ppcre #:closer-mop)
  :serial t
  :components ((:file "package")
               (:file "resting")))

(asdf:defsystem :resting-tests
  :depends-on (#:resting #:fiasco)
  :serial t
  :components ((:file "resting-tests")))
