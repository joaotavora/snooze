(cl:defpackage :clacktest (:use #:cl))
(in-package :clacktest)

(defmethod snooze-backend:backend-class ((key (eql :clack)))
  'clack-rack)

(defclass clack-rack ()
  ((handler :initform nil :accessor handler)
   (server :initarg snooze-backend:server :accessor server)
   (clackup-initargs :accessor clackup-initargs)
   (server-port :accessor server-port)))

(defmethod initialize-instance :after ((obj clack-rack) &rest args &key &allow-other-keys)
  (setf (clackup-initargs obj) args))

(defmethod snooze-backend:start (server (backend clack-rack))
  (when (snooze-backend:started-p server backend)
    (error "Clack-rack backend ~a already started" backend))
  (setf (handler backend)
        (apply #'clack:clackup (lambda (env)
                                 (clack-dispatcher env server))
               (clackup-initargs backend))))

(defmethod snooze-backend:stop (server (backend clack-rack))
  (unless (snooze-backend:started-p server backend)
    (error "Clack-rack backend ~a already stopped" backend))
  (clack:stop (handler backend))
  (setf (handler backend) nil))

(defmethod snooze-backend:started-p (server (backend clack-rack))
  (handler backend))

(defun clack-dispatcher (env server)
  (let ((script-name (getf env :path-info))
        (query-string (or (getf env :query-string) ""))
        (method (getf env :request-method))
        (accept (gethash "accept" (getf env :headers) "*/*"))
        (content-type (getf env :content-type)))
    (setf (server-port (snooze::backend server))
          (getf env :server-port))
    (block exit
      (handler-bind ((snooze:http-condition (lambda (c)
                                              (when snooze:*always-explain-conditions*
                                                (return-from exit
                                                  `(,(snooze:status-code c)
                                                    `(:content-type ,"text/plain")
                                                    (,(snooze:explain-condition server c))))))))
        `(200
          (:content-type ,"text/plain")
          (,(handle-request server method script-name query-string accept content-type)))))))

(defgeneric handle-request (server method script-name query-string accepts-header content-type))
(defmethod handle-request (server method script-name query-string accepts-header content-type)
  (multiple-value-bind (resource args content-class-in-uri)
      (snooze-utils:parse-uri script-name
                              query-string
                              server)
    (let* ((snooze-backend:*current-server* server)
           (content-class
             (or content-class-in-uri
                 (snooze-utils:parse-content-type-header content-type)))
           (verb (snooze-utils:find-verb-or-lose method))
           (converted-arguments (snooze:convert-arguments server resource args)))
      (cond ((not resource)
             (error 'snooze:no-such-resource
                    :format-control
                    "So sorry, but that URI doesn't match any REST resources"))
            ((not (snooze-utils:arglist-compatible-p resource converted-arguments))
             (error 'snooze:invalid-resource-arguments
                    :format-control
                    "Too many, too few, or unsupported query arguments for REST resource ~a"
                    :format-arguments
                    (list resource)))
            (t
             (etypecase verb
               ;; For the Accept: header
               (snooze-verbs:sending-verb
                (let* ((prefiltered-accepted-ct-classes
                         (if content-class-in-uri
                             (list content-class-in-uri)
                             (snooze-utils:prefilter-accepts-header
                              accepts-header
                              resource)))
                       (retval))
                  (loop do (unless prefiltered-accepted-ct-classes
                             (error 'snooze:no-matching-content-types
                                    :accepted-classes prefiltered-accepted-ct-classes))
                          thereis
                          (block try-again
                            (handler-bind ((snooze:no-such-route
                                             #'(lambda (c)
                                                 (declare (ignore c))
                                                 (return-from try-again nil))))
                              ;; autosetting the reply's content-type
                              ;; before the method call gives the
                              ;; route a chance to override it.
                              ;;
                              (let ((content-type (pop prefiltered-accepted-ct-classes)))
                                (setf (hunchentoot:content-type*)
                                      (string (class-name content-type)))
                                (setq retval
                                      (apply resource
                                             verb
                                             ;; FIXME: maybe use singletons here
                                             (make-instance (class-name content-type))
                                             converted-arguments))
                                t))))
                  retval))
               (snooze-verbs:receiving-verb
                (apply resource
                       verb
                       ;; FIXME: no content class in the client's request,
                       ;; 
                       (make-instance (if content-class
                                          (class-name content-class)
                                          'snooze-types:content) 
                                      :content-body
                                      (snooze:request-body))
                       converted-arguments))))))))


