(in-package #:snooze-backend)


;;; Implementation-facing API
;;;
(defgeneric start (server backend)
  (:documentation "How to start running SERVER on BACKEND."))

(defgeneric stop (server backend)
  (:documentation "How to stop running SERVER on BACKEND."))

(defgeneric started-p (server backend)
  (:documentation "How to tell if still running SERVER on BACKEND."))

(defgeneric backend-class (keyword)
  (:documentation "The CLOS class of backend for KEYWORD."))
