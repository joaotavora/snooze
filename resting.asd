(asdf:defsystem :resting
  :depends-on (#:alexandria #:hunchentoot)
  :serial t
  :components ((:file "package")
               (:file "resting")))
