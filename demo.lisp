(defpackage #:snooze-demo (:use #:cl #:snooze)
            (:export
             #:stop
             #:start)
            (:import-from #:cl-who
                          #:htm
                          #:fmt
                          #:str
                          #:with-html-output
                          #:with-html-output-to-string))
(in-package #:snooze-demo)

(defvar *template-stream*)
(defmacro deftemplate (name (&rest lambda-list-args) &body who-args)
  "A micro HTML-templating framework"
  (alexandria:with-gensyms (body function)
    `(progn
       (setf (get ',name 'deftemplate)
             (lambda (,function ,@lambda-list-args)
               (macrolet ((yield () '(funcall ,function)))
                 (with-html-output (*template-stream* nil :indent t)
                   ,@who-args))))

       (defmacro ,name ((stream &rest args ,@lambda-list-args) &body ,body)
         (declare (ignore ,@(loop for sym in lambda-list-args
                                  unless (eq #\& (char (string sym) 0))
                                    collect sym)))
         
         `(apply (get ',',name 'deftemplate)
                 (lambda (,stream)
                   (let ((*template-stream* ,stream))
                     (with-html-output (,stream nil :indent t)
                     ,@,body)))
                 ,(cons 'list args))))))



(deftemplate with-basic-page (&key title)
  (:html
   (:head (:title (str (or title "Snooze")))
          (:meta :name "viewport" :content "width=device-width, initial-scale=1")
          (:link :rel "stylesheet" :href "http://yui.yahooapis.com/pure/0.6.0/pure-min.css")
          (:link :href "/snooze.css" :rel "stylesheet" :type "text/css"))
   (:body
    (:div :id "layout" :class "pure-g coiso"
          (:div :class "pure-u-1-6 pure-menu"
                (:span :class "demo-menu-heading pure-menu-heading" "Symbols")
                (:ul :class "pure-menu-list demo-menu"
                     (loop for sym being the external-symbols of :cl
                           repeat 100
                           do (htm
                               (:li :class "pure-menu-item"
                                    (:a :href (sym-path sym) :class "pure-menu-link"
                                        (fmt "~a" sym)))))))
          (:div :class "content pure-u-5-6"
                (:div :class "pure-menu-heading demo-description" "Description")
                (:div :class "inner-content symdesc"
                      (:div :class "" (yield))
                      (:textarea :class "edit-desc" :row "4" )))))))

(defroute home (:get "text/html")
  (with-basic-page (s :title "Snooze demo")
    (:p :class "main" "Incredible home!")))

(defresource sym (verb ct symbol)
  (:genpath sym-path))

(defroute sym (:get "text/html" sym)
  (let ((doc (documentation sym 'function)))
    (with-basic-page (s :title (symbol-name sym))
      (if doc
          (htm (:pre (format s (cl-who:escape-string-minimal doc))))
          (htm (:p :class "main"
                   (fmt "There's no doc for ~a" sym)))))))

(defmethod uri-to-arguments ((resource (eql #'sym)) uri)
  (handler-case (call-next-method)
    (unconvertible-argument (e)
      (http-condition 404 "No such symbol under ~a" (unconvertible-argument-value e)))))

(defmethod explain-condition :around (c resource (ct snooze-types:text/html))
  (declare (ignore resource c))
  (with-basic-page (s :title "Snooze error")
    (:p :class "main"
        (call-next-method))))

(defmethod explain-condition ((c error) resource (ct snooze-types:text/html))
  (with-html-output (*template-stream*)
    (:i "An unexpected internal error has occured")
    (:pre (str (explain-condition c resource 'snooze::full-backtrace)))))

(defmethod explain-condition ((c http-condition) resource (ct snooze-types:text/html))
  (with-html-output (*template-stream*)
    (:i (fmt "You have a ~a: ~a" (status-code c) c))
    (:pre (str (explain-condition c resource 'snooze::full-backtrace)))))

(defroute snooze (:get "text/css")
  (cl-css:css
   `((.demo-menu-heading :background tomato)
     (.demo-description  :background honeydew)
     (.symdesc :padding 10%
               :padding-top 0
               :padding-bottom 0)
     (.edit-desc :width 100%)
     (.pure-menu-item :height auto)
     (.demo-menu :overflow-y scroll
                 :overflow-x hidden
                 :height 100%)
     (.main :padding 20%
            :text-align center))))


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









