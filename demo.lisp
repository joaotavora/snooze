(defpackage #:snooze-demo (:use #:cl #:snooze)
            (:export
             #:stop
             #:start))
(in-package #:snooze-demo)

(defmacro deftemplate (name (&rest lambda-list-args) &body who-args)
  "A micro HTML-templating framework"
  (alexandria:with-gensyms (body whots-stream function)
    `(progn
       (setf (get ',name 'deftemplate)
             (lambda (,function ,@lambda-list-args)
               (macrolet ((yield () '(funcall ,function ,whots-stream)))
                 (cl-who:with-html-output-to-string (,whots-stream nil :indent t)
                   ,@who-args))))

       (defmacro ,name ((stream &rest args ,@lambda-list-args) &body ,body)
         (declare (ignore ,@(loop for sym in lambda-list-args
                                  unless (eq #\& (char (string sym) 0))
                                    collect sym)))
         `(apply (get ',',name 'deftemplate)
                 (lambda (,stream)
                   (cl-who:with-html-output (,stream nil :indent t)
                     ,@,body))
                 ,(cons 'list args))))))



(deftemplate with-basic-page (&key title)
  (:html
   (:head (:title (cl-who:str (or title "Snooze")))
          (:meta :name "viewport" :content "width=device-width, initial-scale=1")
          (:link :href "/snooze.css" :rel "stylesheet" :type "text/css")
          (:link :rel "stylesheet" :href "http://yui.yahooapis.com/pure/0.6.0/pure-min.css"))
   (:body
    (:div :id "layout" :class "pure-g coiso"
          (:div :class "pure-u-1-3 pure-menu custom-restricted-width"
                (:span :class "pure-menu-heading" "Snooze")
                (:ul :class "pure-menu-list"
                     (loop repeat 3
                           for i from 0
                           do (cl-who:htm
                               (:li :class "pure-menu-item"
                                    (:a :href "#" :class "pure-menu-link"
                                        (cl-who:fmt "item ~a" i)))))))
          (:div :class "pure-u-2-3"
                (yield))))))

(defroute home (:get "text/html")
  (with-basic-page (s :title "Snooze demo")
    (:p "Incredible home!")))

(defroute home (:get "text/html")
  (with-basic-page (s :title "Snooze demo")
    (:p "Incredible home!")))

(defmethod explain-condition ((c error) resource (ct snooze-types:text/html))
  (declare (ignore resource))
  (with-basic-page (s :title "Snooze error")
    (:p (:i (cl-who:fmt "An unexpected internal error has occured")))))

(defmethod explain-condition ((c http-condition) resource (ct snooze-types:text/html))
  (declare (ignore resource))
  (with-basic-page (s :title "Snooze error")
    (:p (cl-who:fmt "ooops you have a ~a" (status-code c)))))

(defroute snooze (:get "text/css")
  (cl-css:css
   `((.custom-restricted-width
      :width 160px
      :font-family serif
      :float left)
     (.coiso
      :height 100px)
     (.pure-menu-item
      :height auto))))


;;; Hook it to Hunchentoot
;;;
(defclass snooze-acceptor (hunchentoot:acceptor)
  ((snooze-bindings :initarg :snooze-bindings :initform nil :accessor snooze-bindings)))

(defmethod hunchentoot:acceptor-dispatch-request ((acceptor snooze-acceptor) request)
  (multiple-value-bind (code payload payload-ct)
      (let (;; Optional, but we do all the error catching ourselves
            ;; 
            (hunchentoot:*catch-errors-p* nil))
        (progv
            (mapcar #'car (snooze-bindings acceptor))
            (mapcar #'cdr (snooze-bindings acceptor))
          (handle-request (hunchentoot:request-uri request)
                          :accept (hunchentoot:header-in :accept request)
                          :method (hunchentoot:request-method request)
                          :content-type (hunchentoot:header-in :content-type request))))
    (setf (hunchentoot:return-code*) code
          (hunchentoot:content-type*) payload-ct)
    (or payload "")))

(defvar *server* nil)

(defun stop ()
  (when *server* (hunchentoot:stop *server*) (setq *server* nil)))

(defun start (&rest args &key (port 5000))
  (stop)
  (setq *server* (hunchentoot:start
                  (apply #'make-instance 'snooze-acceptor
                         :port port args))))









