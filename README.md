Snooze
=======

_Snooze_ is a framework for building REST web services in Common Lisp. 

Here's a small sample:

```lisp
(snooze:defroute describe-sym (:get "text/plain" symbol &key (package :cl))
  (let ((resource (find-symbol (string symbol) package)))
    (if resource
        (with-output-to-string (s) (describe resource s))
        (snooze:http-condition 404 "Sorry, no ~a in ~a" symbol package))))

(clack:clackup (make-clack-app) :port 9003)
```

That's it: no regexps to setup or funny syntaxes to learn. Write your
REST routes like you would write a Lisp function.

Try out these routes:

```
http://localhost:9003/describe-sym/defun
http://localhost:9003/describe-sym/defroute?package=snooze
http://localhost:9003/describe-sym/funny-syntax?package=snooze
```


Rationale
---------

_Snooze_ models HTTP and the operations of Representation State
Transfer (REST) as **function calls** on resources.

It maps *HTTP concepts* like resources, HTTP verbs, content-types, URI
queries, and status codes to *Common Lisp concepts* like generic
functions, specialized-lambda-lists, and conditions.

It relieves the programmer of:

* Manually dispatching on HTTP methods and content-types;

* Parsing the URI to find arguments;

* Writing checks for common situations 400-like situations like
missing resources, content-types mismatch;

* Writing functions that generates compatible URI's for use in HTML.

There are other such systems for Common Lisp, but they tend to make
you learn extra route-definition syntax.

But in _Snooze_ `defroute` is very close to (and completely compatible
with) `defmethod`. In fact you can even use `defmethod` if you prefer.

So, for example, `GET`ting the list of the Beatles in JSON format by
order of most guitars owned is asking for the URI
`/beatles?sort-by=number_of_guitars`, which matches the following
function:

```lisp
(snooze:defroute beatles (:get "application/json" &key (sort-by #'age))
  (jsonify (sort #'> (all-the-beatles) :key sort-by)))
```

This is the same as writing:

```lisp
(cl:defmethod beatles ((v snooze-verbs:get) (c snooze-types:application/json) &key (sort-by #'age))
  (jsonify (sort #'> (all-the-beatles) :key sort-by)))

```

Content-type matching is automatically taken care of: only
`application/json`-accepting requests are accepted by this
snippet. Argument parsing in the URI, including `?param=value` and
`#fragment` bits is also automatically handled (but can be customized
if you don't like the default).


Because it's all done with CLOS, every route is a method, so you can.

* `cl:trace` it like a regular function
* find its definition with `M-.`
* reuse other methods using `call-next-method`
* use `:after`, `:before` and `:around` qualifiers
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

(clack:clackup (make-clack-app) :port 9003)
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
(snooze:defroute todo (:put (payload "text/plain") id)
  (let ((todo (find-todo-or-lose id)))
    (setf (todo-task todo) (request-payload))))
```

To accept any kind of text, not just plain text, use `text/*` as a
specializer.

```lisp
(snooze:defroute todo (:put (payload "text/*") id)...)
```

You can still keep your specialized `text/plain`: it'll get called to
handle plaintext specially, perhaps delegating to the more generic
version using `call-next-method`.

Now to make the route accept JSON content:

```lisp
(snooze:defroute todo (:put (payload "application/json") id)
  (let ((todo (find-todo-or-lose id)))
    (setf (todo-task todo)
          (cdr (assoc :task
                      (json:decode-json-from-string
                       (request-payload)))))))
```

There is a little bit of repetition there. Perhaps its better to find
the `todo` by its `id` number just once.

```lisp
(snooze:defroute todo :around (verb payload id)
  (call-next-method verb payload (find-todo-or-lose id)))

(snooze:defroute todo (:put (payload "text/plain") todo)
  (setf (task todo) (request-payload)))

(snooze:defroute todo (:put (payload "application/json") todo)
  (setf (task todo)
        (cdr (assoc :task
                    (json:decode-json-from-string
                     (request-payload))))))
```

Tighter methods
---------------

If you don't like the `:around` trick, there's a tighter way to
control how arguments get converted before being passed to routes,
using `snooze:convert-arguments`

```lisp
(defmethod snooze:convert-arguments ((resource (eql #'todo)) plain-args keyword-args)
  ;; first, use the default spec to convert any strings to numbers
  ;; 
  (multiple-value-setq (plain-args keyword-args) (call-next-method))
  ;; now, convert the first number to an object, keep everything else untouched
  ;; 
  (values
     (cons (find-todo-or-lose (first plain-args)) (rest plain-args))
     keyword-args))

(snooze:defroute todo (:put (payload "text/plain") (x todo))
  (setf (task x) (request-payload)))

(snooze:defroute todo (:put (payload "application/json") (x todo))
  (setf (task x)
        (cdr (assoc :task
                    (json:decode-json-from-string
                     (request-payload))))))
```


More tricks and URI generation
-----------------------------------

Another trick is to coalesce all the `defroute` definitions into a
single `defresource` definitions, much like `defmethod` can be in a
`defgeneric`:

```lisp
(snooze:defresource todo (verb content-type id)
  (:genpath todo-path)
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

Using `defresource` gives you another bonus, in the form of an
URL-generating function for free, in this case `todo-url`, to use in
your view code:

```
SNOOZE-DEMO> (todo-url 3)
"todo/3"
```

Fancier URI generation
----------------------

If you add `&key` and `&optional` arguments to the route, not only are
they used when dispatching for routes, but can be used to generate
URI.Consider a route that lets you filter the `todo` items:

```
(snooze:defresource todos (verb content-type &key from to substring)
  (:genpath todos-path)
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

The function that you get for free is now `todos-path` and does this:

```
SNOOZE-DEMO> (todos-path :from 3 :to 6 :substring "d")
"todos/?from=3&to=6&substring=d"
SNOOZE-DEMO> (todos-path :from 3)
"todos/?from=3"
SNOOZE-DEMO> (todos-path)
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
