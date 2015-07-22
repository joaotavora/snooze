(in-package #:snooze-backend)


;;; Implementation-facing API
;;;
(defgeneric start (server backend)
  (:documentation "How to start running SERVER on BACKEND."))

(defgeneric stop (server backend)
  (:documentation "How to stop running SERVER on BACKEND."))

(defgeneric started-p (server backend)
  (:documentation "How to tell if still running SERVER on BACKEND."))

(defgeneric request-body (backend)
  (:documentation "Return the current client request's body"))

(defgeneric backend-class (keyword)
  (:documentation "The CLOS class of backend for KEYWORD."))

(defvar *current-server*)
(setf (documentation '*current-server* 'variable)
      "Backends must set this to the current server while handling the request")


