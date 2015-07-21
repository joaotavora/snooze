Snooze
=======

Snooze is for RESTing in Common Lisp.

It's a framework for setting up routes and links to them. It invites
you to use plain old CLOS generic functions to define routes:

* matching HTTP verbs
* matching headers `Content-Type` or `Accept`, according to the verb
* matching certain URL patterns

It's all done with CLOS, so every route is a method that you can
trace, `M-.` into, etc. There's a `defroute` macro for some syntactic
sugar, but it isn't even mandatory.

Snooze is currently based on the great [Hunchentoot][hunchentoot], but
I'd welcome [pull requests][issues] that make it plug into other web
server.

Up and running
--------------

This assumes you're using a recent version of [quicklisp][quicklisp]

```lisp
(push "path/to/snoozes/parent/dir" quicklisp:*local-project-directories*)
(ql:quickload :snooze)
```

or alternatively, just use [asdf][asdf]

```lisp
(push "path/to/snoozes/dir" asdf:*central-registry*)
(asdf:require-system :snooze)
```

now create some Lisp file with

```lisp
(defpackage :snooze-demo (:use :cl))
(in-package :snooze-demo)

(defvar *todo-counter* 0)

(defclass todo ()
  ((id :initform (incf *todo-counter*) :accessor todo-id)
   (task :initarg :task :accessor todo-task)
   (done :initform nil :initarg :done :accessor todo-done)))

(defparameter *todos* 
  (list (make-instance 'todo :task "Wash dishes")
        (make-instance 'todo :task "Scrub floor")
        (make-instance 'todo :task "Doze off" :done t)))

(defun find-todo-or-lose (id)
  (or (find id *todos* :key #'todo-id)
      (error 'snooze:404)))

(snooze:defroute todo (:get "text/plain" id)
  (let ((todo (find-todo-or-lose id)))
    (format nil "~a: ~a ~a" (todo-id todo) (todo-task todo)
            (if (todo-done todo) "DONE" "TODO"))))

(snooze:defroute todos (:get "text/plain")
  (format nil "~{~a~^~%~}" (mapcar #'todo-task *todos*)))

(snooze:start (make-instance 'snooze:snooze-server
                             :port 4242 :route-packages '(:snooze-demo)))
```

And connect to "http://localhost:4242/todos" to see a list of
`todo`'s.

CLOS-based tricks
-----------------

Routes are really only CLOS methods: `defroute` little more than
`defmethod`. For example, to remove particular route just delete the
particular method.

Here's the method that updates a `todo`'s data using a PUT request


```
(snooze:defroute todo (:put content id)
  (let ((todo (find-todo-or-lose id)))
    (setf (todo-task todo)
          (snooze:content-body content))))
```

In this snippet, `content` could be `"application/json"` to make the
server also accept JSON input.

Another trick is to coalesce all the `defroute` definitions into a
single `defresource` definitions, much like `defmethod` can be in a
`defgeneric`:

```lisp
(snooze:defresource todo (verb content-type id)
  (:genurl todo-url)
  (:route (:get "text/*" id)
          (todo-task (find-todo-or-lose id)))
  (:route (:get "text/html" id)
          (format nil "<b>~a</b>" (call-next-method)))
  (:route (:get "application/json" id)
          ;; you should use some json-encoding package here, tho
          (let ((todo (find-todo-or-lose id)))
            (format nil "{id:~a,task:~a,done:~a}"
            (todo-id todo) (todo-task todo) (todo-done todo)))))
```

Using `defresource` gives you another bonus, which is you get to
specify an URL-generating function, in this case `todo-url`, to use in
your view code:

```
SNOOZE-DEMO> (todo-url 3)
"todo/3"
SNOOZE-DEMO> (todo-url 3 :protocol "https" :host "localhost")
"https://localhost/todo/3"
```

What happens if I add &optional or &key?
----------------------------------------

That's a very good question, and thanks for asking :grin:

Controlling errors
------------------

TODO...

Support
-------

To discuss matters open an [issue][issues] for now or perhaps ask in
the [#lisp][sharp-lisp] IRC channel.


[quicklisp]: http://quicklisp.org
[asdf]: http://common-lisp.net/project/asdf/
[hunchentoot]: https://github.com/edicl/hunchentoot
[sharp-lisp]: irc://irc.freenode.net/#lisp
[issues]: https://github.com/capitaomorte/snooze/issues
