Snooze
=======

_Snooze_ is a framework for building REST web services in Common Lisp. 

Here's a very short sample with a `GET` and `PUT` routes.

```lisp
(defpackage #:readme-demo (:use #:cl #:snooze))
(in-package #:readme-demo)

(defun find-symbol-or-lose (name package)
  (or (find-symbol (string name) (find-package package))
      (http-condition 404 "Sorry, no such symbol")))

(defroute lispdoc (:get "text/*" name &key (package :cl) (type 'function))
  (or (documentation (find-symbol-or-lose name package) type)
      (http-condition 404 "Sorry no ~a doc for ~a" type name)))

(defroute lispdoc (:put "text/plain" name &key (package :cl) (type 'function))
  (setf (documentation (find-symbol-or-lose name package) type)
        (payload-as-string)))

(clack:clackup (snooze:make-clack-app) :port 9003)
```

No regular expressions or funny syntax: routes not only *look like*
functions, they *are* functions.

Now you get your routes and also some error reporting for free:

```HTTP
GET /lispdoc/defun                         => 200 OK
GET /lispdoc/funny-syntax?package=snooze   => 404 Not found
GET /lispdoc/in/?valid=args                => 400 Bad Request
                                           
GET /lispdoc/defun                         => 406 Not Acceptable 
Accept: application/json

GET /lispdoc/defun                         => 200 OK (can serve any text)
Accept: application/json,text/html
                                           
PUT /lispdoc/scan?package=cl-ppcre         => 200 OK 
Content-type: text/plain

PUT /lispdoc/defun                         => 415 Unsupported Media Type 
Content-type: application/json

DELETE /lispdoc/defun                      => 501 Not implemented
GET /lispdoc/scan?package=gibberish        => 500 Internal Server Error
```

Read on for the rationale, or checkout the [tutorial](#tutorial)

Rationale
---------

_Snooze_ relieves the programmer of writing application code to:

* dispatch on HTTP methods and content-types;
* decode the URI to find arguments;
* check for common 400-like situations;
* generate and encode a compatible URIs;
* politely handle failure conditions;

There are other such systems for Common Lisp, but they tend to make
you learn [extra](https://github.com/fukamachi/caveman#routing)
[route-definition](http://restas.lisper.ru/en/manual/routes.html#routes)
[syntax](http://8arrow.org/ningle/).

_Snooze_ maps [REST/HTTP](https://en.wikipedia.org/wiki/REST) concepts
to Common Lisp concepts:

| HTTP concept                     | Common Lisp concept       |
| :------------------------------- | -------------------------:|
| verbs (`GET`, `PUT`, `DELETE`, `etc`)  | CLOS argument specializer |
| `Accept:` and `Content-Type:`    | CLOS argument specializer |
| URL queries (`?param=value&p2=v2`) | keyword arguments         |
| status codes (`404`, `500`, etc)     | conditions                |

Because it's all CLOS, every route is a method, so you can.

* `cl:trace` it like a regular function
* find its definition with `M-.`
* reuse other methods using `call-next-method`
* use `:after`, `:before` and `:around` qualifiers
* delete the route by deleting the method

Tutorial
--------

This assumes you're using a recent version of [quicklisp][quicklisp]

```lisp
(push "path/to/snoozes/parent/dir" quicklisp:*local-project-directories*)
(ql:quickload :snooze)
```

Consider the sample [above](#snooze) and let's pick up where we left
off. We start by serving docstrings in HTML. Because routes are really
only CLOS methods, the easiest way is:

```lisp
(defroute lispdoc :around (:get "text/html" name &key &allow-other-keys)
  (format nil "<h1>Docstring for ~a</h1><p>~a</p>"
          name
          (call-next-method)))
```

Though you should probably escape the HTML with something like
[cl-who:escape-string-all](http://weitz.de/cl-who/#escape-string-all).

Now let's accept `PUT` requests with JSON content:

```
(defroute lispdoc (:put "application/json" name &key package doctype)
  (let* ((json (json:decode-json-from-string
                (payload-as-string)))
         (doctype (or (cdr (assoc :doctype json)) doctype))
         (package (or (cdr (assoc :package json)) package))
         (sym (find-symbol-or-lose name package))
         (docstring (cdr (assoc :docstring json))))
    (if (and sym docstring doctype)
        (setf (documentation sym doctype) docstring)
        (http-condition 400 "Not acceptable"))))
```

Tighter routes
---------------

The 4 routes we have until now all use the `find-symbol-or-lose`
helper, and they also have that `package` keyword arg. They could be
simply functions of a symbol. After all, Lisp already has a pretty
good way of designating symbols and packages using the `:` colon
syntax.

Wouldn't it be nicer if we could write routes like:

```lisp
(defroute lispdoc (:get "text/*" symbol &key (doctype 'function))
  (or (documentation symbol doctype)
      (http-condition 404 "Sorry no ~a doc for ~a" doctype symbol)))
```

This will work just fine as long as the client asks for routes like
`lispdoc/common-lisp%3Adefun` or `lispdoc/snooze%3Adefroute` and *not*
the more human-readable REST routes we had above.

Because we might have already published these routes to the world, we
need to change the implementation without changing the interface.





 We can use `defresourc` to get a
generator


These
are not very easily human readable

But what if we've already published the



```lisp
(defmethod snooze:uri-to-arguments ((resource (eql #'todo)) uri)
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

```lisp
SNOOZE-DEMO> (todo-url 3)
"todo/3"
```

Fancier URI generation
----------------------

If you add `&key` and `&optional` arguments to the route, not only are
they used when dispatching for routes, but can be used to generate
URI.Consider a route that lets you filter the `todo` items:

```lisp
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

```lisp
SNOOZE-DEMO> (todos-path :from 3 :to 6 :substring "def")
"todos/?from=3&to=6&substring=%22def%22"
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
