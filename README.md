Resting
=======

Resting is RESTful framework for Hunchentoot based web servers.

It invites you to use plain old CLOS generic functions to define routes:

* matching HTTP verbs
* matching headers `Content-Type` or `Accept`, according to the verb
* matching certain URL patterns

It's all done with CLOS, so every route is a method that you can
trace, `M-.` into, etc. There's a `defroute` macro for some syntactic
sugar, but it isn't even mandatory.

Up and running
--------------

This assumes you're using a recent version of [quicklisp][quicklisp]

```lisp
(push "path/to/restings/parent/dir" quicklisp:*local-project-directories*)
(ql:quickload :resting)
```

or alternatively, just use [asdf][asdf]

```lisp
(push "path/to/restings/dir" asdf:*central-registry*)
(asdf:require-system :resting)
```

now create some Lisp file with

```lisp
(defpackage :resting-demo (:use :cl))
(in-package :resting-demo)

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

(resting:defroute todo (:get "text/plain" id)
  (let ((todo (find id *todos* :key #'todo-id)))
    (if todo
        (todo-task todo)
        (error 'resting:404))))

(resting:defroute todos (:get "text/plain")
  (format nil "~{~a~^~%~}" (mapcar #'todo-task *todos*)))

(hunchentoot:start (make-instance 'resting:rest-acceptor :port 4242))
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
[sharp-lisp]: irc://irc.freenode.net/#lisp
[issues]: https://github.com/capitaomorte/resting/issues
