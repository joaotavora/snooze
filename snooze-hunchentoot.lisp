;;; Hunchentoot Snooze implementation
;;;
(cl:defpackage #:snooze-hunchentoot
  (:use :cl))

(in-package #:snooze-hunchentoot)

(defclass rest-acceptor (hunchentoot:acceptor)
  ((server :initarg snooze-backend:server :initform (error "required!") :accessor server)))

(defmethod snooze-backend:start (server (backend rest-acceptor))
  (declare (ignore server))
  (hunchentoot:start backend))

(defmethod snooze-backend:stop (server (backend rest-acceptor))
  (declare (ignore server))
  (hunchentoot:stop backend))

(defmethod snooze-backend:started-p (server (backend rest-acceptor))
  (declare (ignore server))
  (hunchentoot:started-p backend))

(defmethod snooze-backend:backend-class ((key (eql :hunchentoot)))
  'rest-acceptor)

(defmethod snooze-backend:request-body ((backend rest-acceptor))
  (hunchentoot:raw-post-data))

(defmethod hunchentoot:acceptor-dispatch-request :around ((acceptor rest-acceptor) request)
  (declare (ignore request))
  (let ((retval (call-next-method)))
    (etypecase retval
      (pathname (hunchentoot:handle-static-file retval))
      (string retval))))

(defmethod hunchentoot:acceptor-dispatch-request ((acceptor rest-acceptor) request)
  (handler-bind ((condition
                   (lambda (c)
                     (setf (hunchentoot:aux-request-value 'explain-condition) c)))
                 (snooze:http-condition
                   (lambda (c)
                     (setf (hunchentoot:return-code*) (snooze:status-code c))
                     (when (or hunchentoot:*catch-errors-p*
                               snooze:*always-explain-conditions*)
                       (hunchentoot:abort-request-handler)))))
    (multiple-value-bind (resource args content-class-in-uri)
        (snooze-utils:parse-uri (hunchentoot:script-name request)
                          (hunchentoot:query-string request)
                          (server acceptor))
      (let* ((snooze-backend:*current-server* (server acceptor))
             (content-class
               (or content-class-in-uri
                   (snooze-utils:parse-content-type-header (hunchentoot:header-in :content-type request))))
             (verb (snooze-utils:find-verb-or-lose (hunchentoot:request-method request)))
             (converted-arguments (snooze:convert-arguments acceptor resource args)))
        (cond ((not resource)
               (if (snooze:fall-through-p (server acceptor))
                   (call-next-method)
                   (error 'snooze:no-such-resource
                          :format-control
                          "So sorry, but that URI doesn't match any REST resources")))
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
                                (hunchentoot:header-in :accept request)
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
                         converted-arguments)))))))))

(defmethod hunchentoot:acceptor-status-message ((acceptor rest-acceptor) status-code
                                                &rest args
                                                &key error backtrace
                                                &allow-other-keys)
  (declare (ignore error backtrace status-code))
  (let* ((condition (hunchentoot:aux-request-value 'explain-condition hunchentoot:*request*)))
    (if condition
        (snooze:explain-condition (server acceptor) condition)
        (call-next-method))))
