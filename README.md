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
   (done :initarg :done :accessor todo-done)))

(defparameter *todos* 
  (list (make-instance 'todo :task "Wash dishes")
        (make-instance 'todo :task "Scrub floor")
        (make-instance 'todo :task "Doze off" :done t)))

(defmethod print-object ((x todo) s)
  (print-unreadable-object (x s)
    (format s "~a \"~a\"" (todo-id x) (todo-task x))))

(snooze:defroute todo (:get "text/plain" id)
  (let ((todo (find id *todos* :key #'todo-id)))
    (if todo
        (todo-task todo)
        (error 'snooze:404))))

(snooze:defroute todos (:get "text/plain")
  (format nil "~{~a~^~%~}" (mapcar #'todo-task *todos*)))

(hunchentoot:start (make-instance 'snooze:rest-acceptor :port 4242))
```

CLOS-based tricks
-----------------

TODO...

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
