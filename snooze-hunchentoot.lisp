;;; Hunchentoot Snooze implementation
;;;
(defpackage #:snooze-hunchentoot
  (:export
   #:parse-uri
   #:code))
(in-package #:snooze-hunchentoot)

(defclass rest-acceptor (hunchentoot:acceptor)
  ((server :initarg :server :initform (error "required!") :accessor server)))

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

(defmethod hunchentoot:acceptor-dispatch-request :around ((acceptor rest-acceptor) request)
  (declare (ignore request))
  (let ((retval (call-next-method)))
    (etypecase retval
      (pathname (hunchentoot:handle-static-file retval))
      (string retval))))

(defmethod hunchentoot:acceptor-dispatch-request ((acceptor rest-acceptor) request)
  (handler-bind ((error
                   (lambda (c)
                     (setf (hunchentoot:aux-request-value 'explain-condition) c)))
                 (snooze:http-condition
                   (lambda (c)
                     (setf (hunchentoot:return-code*) (snooze:code c))
                     (when (or hunchentoot:*catch-errors-p*
                               snooze:*always-explain-conditions*)
                       (hunchentoot:abort-request-handler)))))
    (multiple-value-bind (resource args content-class)
        (snooze:parse-uri (hunchentoot:script-name request)
                          (hunchentoot:query-string request)
                          (server acceptor))
      (let* ((content-class
               (or content-class
                   (snooze:parse-content-type-header (hunchentoot:header-in :content-type request))))
             (verb-designator (or (snooze:probe-class-sym
                                   (intern (string-upcase (hunchentoot:request-method request))
                                           :snooze-verbs))
                                  (error "Can't find HTTP verb designator for request ~a!" request)))
             ;; FIXME: maybe use singletons here
             (verb (and verb-designator
                        (make-instance verb-designator))) 
             (converted-arguments (snooze:convert-arguments acceptor resource args))
             (accepted-classes
               (if content-class (list content-class)
                   (snooze:parse-accept-header (hunchentoot:header-in :accept request)
                                               (server acceptor)
                                               resource))))
        (cond ((not resource)
               (if (snooze:fall-through-p (server acceptor))
                   (call-next-method)
                   (error 'snooze:no-such-route
                          :format-control
                          "So sorry, but that URI doesn't match any REST resources")))
              ((not (snooze:arglist-compatible-p resource converted-arguments))
               (error 'snooze:no-such-route
                      :format-control
                      "Too many, too few, or unsupported query arguments for REST resource ~a"
                      :format-arguments
                      (list resource)))
              (t
               (etypecase verb
                 ;; For the Accept: header
                 (snooze-verbs:sending-verb
                  (let ((try-list accepted-classes)
                        (retval))
                    (loop do (unless try-list
                               (error 'snooze:no-matching-content-types
                                      :accepted-classes accepted-classes
                                      :format-control
                                      "No matching routes for verb~%  ~a~%on resource~%  ~a~%and accept list~%  ~a"
                                      :format-arguments
                                      (list verb resource
                                            (mapcar #'class-name accepted-classes))))
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
                                (let ((content-type (pop try-list)))
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
                         (make-instance (class-name content-class) 
                                        :content-body
                                        (hunchentoot:raw-post-data :request request))
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
