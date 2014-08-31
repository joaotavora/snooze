(asdf:defsystem :resting
  :depends-on (#:alexandria #:hunchentoot #:cl-ppcre #:closer-mop)
  :serial t
  :components ((:file "package")
               (:file "resting")))
