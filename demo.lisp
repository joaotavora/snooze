(defpackage #:snooze-demo (:use #:cl #:snooze))
(in-package #:snooze-demo)

(defmacro deftemplate (name (&rest lambda-list-args) &body who-args)
  (let ((function-sym (gensym "FUNCTION-"))
        (whots-var-sym (gensym "WHOTS-VAR-"))
        (body-sym (gensym "BODY-")))
    `(progn
       (setf (get ',name 'deftemplate)
             (lambda (,function-sym ,@lambda-list-args)
               (macrolet ((yield () '(funcall ,function-sym ,whots-var-sym)))
                 (cl-who:with-html-output-to-string (,whots-var-sym nil :indent t)
                   ,@who-args))))

       (defmacro ,name ((stream &rest args ,@lambda-list-args) &body ,body-sym)
         (declare (ignore ,@(loop for sym in lambda-list-args
                                  unless (eq #\& (char (string sym) 0))
                                    collect sym)))
         `(apply (get ',',name 'deftemplate)
                 (lambda (,stream)
                   (cl-who:with-html-output (,stream nil :indent t)
                     ,@,body-sym))
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

(defmethod explain-condition ((c error) (ct snooze-types:text/html))
  (with-basic-page (s :title "Snooze error")
    (:p (:i (cl-who:fmt "An unexpected internal error has occured")))))

(defmethod explain-condition ((c http-condition) (ct snooze-types:text/html))
  (with-basic-page (s :title "Snooze error")
    (:p (cl-who:fmt "ooops you have a ~a" (status-code c)))))

(defroute snooze (:get "text/css")
  (cl-css:css
   `((.custom-restricted-width
      :width 160px
      :font-family serif
      :float left)
     (.coiso
      :height 100px))))


