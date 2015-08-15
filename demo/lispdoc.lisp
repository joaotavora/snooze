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

(defmacro deftemplate (name (stream-var &rest lambda-list-args) &body who-args)
  "A micro HTML-templating framework"
  (alexandria:with-gensyms (body function)
    `(progn
       (setf (get ',name 'deftemplate)
             (lambda (,function ,stream-var ,@lambda-list-args)
               (macrolet ((yield () '(funcall ,function)))
                 (with-html-output (,stream-var nil :indent t)
                   ,@who-args)
                 nil)))

       (defmacro ,name ((stream &rest args ,@lambda-list-args) &body ,body)
         (declare (ignore ,@(loop for thing in lambda-list-args
                                  for sym = (if (consp thing) (car thing) thing)
                                  unless (eq #\& (char (string sym) 0))
                                    collect sym)))
         `(apply (get ',',name 'deftemplate)
                 (lambda ()
                   (with-html-output (,stream nil :indent t)
                     ,@,body))
                 ,stream
                 ,(cons 'list args))))))


(defresource lispdoc (verb ct symbol) (:genpath lispdoc-path))

(defresource homepage (verb ct))

(defun render-pretty-date(s stamp)
  (let* ((now (local-time:now))
         (diff (local-time-duration:timestamp-difference now stamp)))
    (loop for type in (reverse '((:sec . "second")
                                 (:minute . "minute")
                                 (:hour . "hour")
                                 (:day . "day")
                                 (:week . "week")))
          for as-type = (local-time-duration:duration-as diff (car type))
          until (plusp as-type)
          finally
             (return (if (plusp as-type)
                         (format s "~a ~a~:*~:*~P ago" as-type (cdr type))
                         (progn (format s "on ~a"
                                        (local-time:format-timestring nil stamp
                                                                      :format local-time:+asctime-format+
                                                                      ))))))))

(defun render-some-symbols (stream &optional (package :cl))
  (with-html-output (stream)
    (:span :class "red-highlight pure-menu-heading"
           (fmt "Symbols of ~a" package))
    (:ul :class "pure-menu-list demo-menu"
         (loop for sym being the external-symbols of package
          ;; repeat 100
          do (htm
              (:li :class "pure-menu-item"
                   (:a :href (lispdoc-path sym) :class "pure-menu-link"
                       (fmt "~a" sym))))))))

(defun md5-as-string (input)
  (format nil "~(~{~2,'0X~}~)"
          (map 'list #'identity
               (md5:md5sum-string
                input))))

(defparameter *doc-changes*
  `(("João" "joaotavora@gmail.com" ,(local-time:now) snooze:defroute ,(documentation 'snooze:defroute 'function))
    ("Really Really Long Name the Third" "joaotavora@gmail.com" ,(local-time:now) snooze:defroute ,(documentation 'snooze:defroute 'function))
    ))

(defun render-recent-changes (stream)
  (with-html-output (stream)
    (:span :class "red-highlight pure-menu-heading" "Recent additions")
    (loop for (name email stamp symbol proposed) in *doc-changes*
          for i from 0
          do (htm
              (:div :class "pure-something" :style (cl-css:inline-css '(border-bottom "1px solid grey"))
                    (:div :class "recent-change-heading" :style (cl-css:inline-css '(height 30px font-size 80% padding 10px))
                          (:img :class "sidebar-avatar pure-img" :src
                                (format nil "http://www.gravatar.com/avatar/~a?s=30"
                                        (md5-as-string
                                         (string-downcase (string-trim #(#\Space #\Newline) email)))))
                          (:div :class "change-name"
                                  (htm (:div (fmt "~a documented " name)
                                              (:a :class "symbol-link" :href (str (lispdoc-path symbol))
                                                  (let ((*package* (find-package :keyword)))
                                                    (fmt "~a" symbol))))
                                       (:div
                                        (fmt "~a" (render-pretty-date nil stamp))))))
                    (:div (:p :class "recent-change recent-change-quote"
                              (fmt "\"~a\"" proposed))))))))

;; (defun render-doc (stream sym doctype)
;;   (declare (ignore doctype))
;;   (with-html-output (stream)
;;     ))

(deftemplate with-basic-page (stream &key
                                     title
                                     (left-render 'render-some-symbols)
                                     (right-render 'render-recent-changes))
  (:html
   (:head (:title (str (or title "Snooze")))
          (:meta :name "viewport" :content "width=device-width, initial-scale=1")
          (:link :rel "stylesheet" :href "http://yui.yahooapis.com/pure/0.6.0/pure-min.css")
          (:link :href "/snooze.css" :rel "stylesheet" :type "text/css"))
   (:body
    (:header :class "pure-g"
             (:div :class "pure-u-1" 
                   (:h3 (:span :style (cl-css:inline-css '(font-style italic color white font-size 110%)) "Docstrung" )
                        (str ", saving lisp one docstring at a time") ))))
   (:section
    (:div :id "layout" :class "pure-g"
          (:div :class "pure-u-5-24 pure-menu"
                (when left-render (funcall left-render stream)))
          (:div :class "content pure-u-14-24"
                (:div :class "pure-menu-heading center-heading secondary-highlight" "Description")
                (:div :class "inner-content" (yield)))
          (:div :class "pure-u-5-24 pure-menu"
                (when right-render (funcall right-render stream)))))
    (:footer :class "pure-g")))

(defroute lispdoc (:get "text/html" (sym symbol))
  (with-output-to-string (s)
    (let ((doc (documentation sym 'function)))
      (with-basic-page (s :title (symbol-name sym))
        (:div :class "main symdesc"
              (if doc
                  (htm (:p (str (cl-who:escape-string-all doc))))
                  (htm (:p (fmt "There's no doc for ~a" sym))))
              (:form :class "pure-form"
               (:legend  (if doc
                             (fmt "Add your take on ~a" sym)
                             "Care to add some?"))
               (:fieldset :class "pure-group"
                          (:textarea :class "pure-input-1"
                                     :placeholder (fmt "Really good docstring for ~a" sym))
                          (:input :type "text" :class "pure-input-1" :placeholder "Your name")
                          (:input :type "text" :class "pure-input-1" :placeholder "Your email. For some gravatar-antics."))
               (:button :type "submit" :class "pure-button pure-input-1 pure-button-primary red-highlight"
                        "Submit!")))))))

(defmethod uri-to-arguments ((resource (eql #'lispdoc)) uri)
  #+allegro (declare (ignore uri))
  (multiple-value-bind (plain-args keyword-args)
      (call-next-method)
    (let* ((sym-name (string (second plain-args)))
           (package-name (string (first plain-args)))
           (sym (find-symbol sym-name package-name)))
      (unless sym
        (http-condition 404 "Sorry, no such symbol"))
      (values (cons sym (cddr plain-args))
              keyword-args))))


(defmethod arguments-to-uri ((resource (eql #'lispdoc)) plain-args keyword-args)
  (let ((sym (first plain-args)))
    (assert (symbolp sym) nil "This only generates paths to symbols")
    (call-next-method resource
                      (list* (read-for-resource resource (package-name (symbol-package sym)))
                             (read-for-resource resource (symbol-name sym))
                             (cdr plain-args))
                      keyword-args)))

(defvar *explain-stream*)

(defmethod explain-condition :around (c (resource (eql #'lispdoc)) (ct snooze-types:text/html))
  #+allegro (declare (ignore c))
  (with-output-to-string (*explain-stream*)
    (with-basic-page (*explain-stream* :title "Snooze error"
                                       ;; :left-render nil
                                       :right-render nil)
      (:p :class "main"
          (call-next-method)))))

(defmethod explain-condition ((c error) (resource (eql #'lispdoc)) (ct snooze-types:text/html))
  (with-html-output (*explain-stream*)
    (:i "An unexpected internal error has occured")
    (:pre (str (snooze::explain-condition-failsafe c resource t)))))

(defmethod explain-condition ((c http-condition) (resource (eql #'lispdoc)) (ct snooze-types:text/html))
  (with-html-output (*explain-stream*)
    (:i (fmt "You have a ~a: ~a" (status-code c) c))))

(defroute snooze (:get "text/css")
  (cl-css:css
   `(("#layout" :height 600px)
     ("html, button, input, select, textarea, .pure-g [class *= \"pure-u\"]"
      :font-family "Georgia, Times, \"Times New Roman\", serif")
     (header :height 60px :background slategrey :color lightgray)
     (footer :height 250px :background slategrey)
     (.red-highlight :background gray)
     (.secondary-highlight :background slategrey )
     (.center-heading :text-align center)
     (.recent-change :height 100px :padding 10px)
     (.recent-change-quote :overflow-y hidden :font-style italic)
     (.change-name :align center :text-align right)
     (.symbol-link :text-decoration none :color darkslategrey)
     (.sidebar-avatar :float left)
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

(defroute homepage (:get "text/html")
  (with-output-to-string (s)
      (with-basic-page (s :title "Docstrung")
        (:div :class "main" (:i "Welcome, click a symbol on the left side")))))

(defun lispdoc-root ()
  (fad:pathname-as-directory
   (make-pathname :name nil
                  :type nil
                  :defaults #.(or *compile-file-truename* *load-truename*))))


;;; Hook it to Hunchentoot
;;;
(defclass snooze-acceptor (hunchentoot:easy-acceptor) ())

(defparameter *lispdoc-dispatch-table*
  (list
   (hunchentoot:create-folder-dispatcher-and-handler
    "/images/" (fad:pathname-as-directory
                (make-pathname :name "images"
                               :defaults (lispdoc-root))))
   (hunchentoot:create-folder-dispatcher-and-handler
    "/js/" (fad:pathname-as-directory
            (make-pathname :name "js"
                           :defaults (lispdoc-root))))
   (hunchentoot:create-folder-dispatcher-and-handler
    "/css/" (fad:pathname-as-directory
            (make-pathname :name "js"
                           :defaults (lispdoc-root))))
   (make-hunchentoot-app )))

(defmethod hunchentoot:acceptor-dispatch-request :around ((a snooze-acceptor) request)
  (let ((hunchentoot:*dispatch-table* *lispdoc-dispatch-table*))
    (call-next-method)))

(defvar *server* nil)

(defun stop ()
  (when *server* (hunchentoot:stop *server*) (setq *server* nil)))

(defun start (&key (port 5000))
  (stop)
  (setq *server* (hunchentoot:start (make-instance 'snooze-acceptor :port port))))


