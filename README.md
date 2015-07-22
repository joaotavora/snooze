Snooze
=======

_Snooze_ is a framework for designing REST web services in Common Lisp. 

Here's a small sample

```lisp
(snooze:defroute probe-symbol (:get "text/plain" symbol-name &key (package :cl))
  (if (and (find-package (string-upcase package))
           (find-symbol (string-upcase symbol-name)
                        (string-upcase package)))
      (format nil "Hello world, package ~a has the symbol ~a" package symbol-name)
      (snooze:http-error 404 "Sorry, no such symbol")))

(snooze:start (make-instance 'snooze:snooze-server :port 9003
                             :route-packages '(:snooze-symbols)))
```

You can now navigate to:

```
http://localhost:9003/probe-symbol/defun
http://localhost:9003/probe-symbol/defroute?package=snooze
```

Rationale
---------

_Snooze_ takes advantage of the fact that REST operations can be seen
as function call on resources. So it uses the readily available CLOS
dispatching mechanisms and maps them to REST concepts like HTTP verbs,
content types to set up routes resources.

So `GET`ting the list of the Beatles in JSON format has the URL
`/beatles` and matches the function:

```lisp
(snooze:defroute beatles (:get "application/json" &key (sort-by #'number-of-guitars))
  (jsonify (all-the-beatles)))
```

URI parameters map directly to `&key` and `&optional` parameters and
there are also URL generator/helpers for view code.

Because it's all done with CLOS, every route is a method:

* you can trace it like a regular function
* find definition its definition with `M-.`
* delete the route by deleting the method

Snooze's only current backend implementation is based on the great
[Hunchentoot][hunchentoot], but I'd welcome [pull requests][issues]
that make it plug into other web server.

Try it out
----------

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


```lisp
(snooze:defroute todo (:put content id)
  (let ((todo (find-todo-or-lose id)))
    (setf (todo-task todo) (snooze:request-body))))
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

Using `defresource` gives you another bonus, which it gives you an
URL-generating function for free, in this case `todo-url`, to use in
your view code:

```
SNOOZE-DEMO> (todo-url 3)
"todo/3"
SNOOZE-DEMO> (todo-url 3 :protocol "https" :host "localhost")
"https://localhost/todo/3"
```

What happens if I add &optional or &key?
----------------------------------------

That's a very good question, and thanks for asking :grin:. They're
allowed, of course, and their values deduced from URI parameters.

Confusing? Consider a route that lets you filter the `todo` items:

```
(snooze:defresource todos (verb content-type &key from to substring)
  (:genurl todos-url)
  (:route (:get "text/plain" &key from to substring)
          (format
           nil "~{~a~^~%~}"
           (mapcan #'todo-task
                   (remove-if-not (lambda (todo)
                                    (and (or (not from)
                                             (> (todo-id todo) from))
                                         (or (not to)
                                             (< (todo-id todo) to))
                                         (or (not substring)
                                             (search substring
                                                     (todo-task todo)))))
                                  *todos*)))))
```

The function that you get for free is now `todos-url` and does this:

```
SNOOZE-DEMO> (todos-url :from 3 :to 6 :substring "d")
"todos/?from=3&to=6&substring=d"
SNOOZE-DEMO> (todos-url :from 3)
"todos/?from=3"
SNOOZE-DEMO> (todos-url)
"todos/"
```

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
