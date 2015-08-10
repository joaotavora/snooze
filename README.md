Snooze
=======

_Snooze_ is a framework for building REST web services in Common Lisp. 

Here's a very simple application to read and write Lisp documentation
via HTTP:

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

No regular expressions, annotations or otherwise funny syntax: routes
not only *look like* functions, they *are* functions.

Here are the routes thus defined and some of the error reporting you
get for free:

```HTTP
GET /lispdoc/defun                         => 200 OK
GET /lispdoc/funny-syntax?package=snooze   => 404 Not found
GET /lispdoc/in/?valid=args                => 400 Bad Request
                                           
GET /lispdoc/defun                         => 406 Not Acceptable 
Accept: application/json

PUT /lispdoc/scan?package=cl-ppcre         => 200 OK 
Content-type: text/plain

PUT /lispdoc/defun                         => 415 Unsupported Media Type 
Content-type: application/json
```

Read on for the rationale, or checkout the [tutorial](#tutorial)

Rationale
---------

_Snooze_ maps [REST/HTTP](https://en.wikipedia.org/wiki/REST) concepts
to Common Lisp concepts:

| HTTP concept                        | Snooze CL concept                   |
| :---------------------------------- | ----------------------------------: |
| Verbs (`GET`, `PUT`, `DELETE`, etc) | CLOS specializer on first argument  |
| `Accept:` and `Content-Type:`       | CLOS specializer on second argument |
| URI path (`/path1/path2/path3)`)    | Required and optional arguments     |
| URL queries (`?param=value&p2=v2`)  | Keyword arguments                   |
| Status codes (`404`, `500`, etc)    | CL conditions                       |

_Snooze_ relieves the programmer of writing application code to:

* dispatch on HTTP methods and content-types;
* decode the URI to find arguments;
* check for common 400-like situations;
* generate and encode a compatible URIs;
* "politely" handle failure conditions;

There are other such systems for Common Lisp, but they tend to make
you learn [extra](https://github.com/fukamachi/caveman#routing)
[route-definition](http://restas.lisper.ru/en/manual/routes.html#routes)
[syntax](http://8arrow.org/ningle/). In _Snooze_ since every route is
a method, you can:

* `cl:trace` it like a regular function
* find its definition with `M-.`
* reuse other methods using `call-next-method`
* use `:after`, `:before` and `:around` qualifiers
* delete the route by deleting the method

Tutorial
--------

Consider the sample [above](#snooze) and let's pick up where we left
off. This tutorial assumes you're using a recent version of
[quicklisp][quicklisp]. Start by entering this into your REPL.

```lisp
(push "path/to/snoozes/parent/dir" quicklisp:*local-project-directories*)
(ql:quickload :snooze)
```

### Content-Types

Let's start by serving docstrings in HTML. As seen above, we already
have a route which matches any text:

```
(defroute lispdoc (:get "text/*" name &key (package :cl) (type 'function))
  (or (documentation (find-symbol-or-lose name package) type)
      (http-condition 404 "Sorry no ~a doc for ~a" type name)))
```

To add HTML support, we just notice that `text/html` *is*
`text/*`. Also because routes are really only CLOS methods, the
easiest way is:

```lisp
(defroute lispdoc :around (:get "text/html" name &key &allow-other-keys)
  (format nil "<h1>Docstring for ~a</h1><p>~a</p>"
          name (call-next-method)))
```

Though you should probably escape the HTML with something like
[cl-who:escape-string-all](http://weitz.de/cl-who/#escape-string-all). You
might also consider specializing directly to `text/html` once you
start needing some more HTML-specific behaviour.

Finally, let's accept `PUT` requests with JSON content. In this
version we accept the `package` and `doctype` parameters in the JSON
request's body.

```lisp
(defroute lispdoc (:put "application/json" name &key package doctype)
  (let* ((json (handler-case
                   (json:decode-json-from-string
                    (payload-as-string))
                 (error (e)
                   (http-condition 400 "Malformed JSON (~a)!" e))))
         (doctype (or (cdr (assoc :doctype json)) doctype))
         (package (or (cdr (assoc :package json)) package))
         (sym (find-symbol-or-lose name package))
         (docstring (cdr (assoc :docstring json))))
    (if (and sym docstring doctype)
        (setf (documentation sym doctype) docstring)
        (http-condition 400 "JSON missing some properties"))))
```

### URI generation

_Snooze_ can generate the URIs, as astring, the URIs you serve in
your application, so we can do this.

(defroute lispdoc (:get "text/*" name &key (package :cl) (type 'function))
  (or (documentation (find-symbol-or-lose name package) type)
      (http-condition 404 "Sorry no ~a doc for ~a" type name)))


The "no doc for <symbol>" message isn't very helpful, particularly
when the client missed looking for `variable` documentation to `*`

It's Another trick is to coalesce all the `defroute` definitions
into a single `defresource` definitions, much like `defmethod` can be
in a `defgeneric`:

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

Tighter routes
---------------

The 4 routes we have until now all use the `find-symbol-or-lose`
helper, and they also have that `package` keyword arg. They could be
simply functions of a symbol. After all, Lisp already has a pretty
good way of designating symbols and packages using the `:` colon
syntax.

It woudl be nicer if we could write routes like:

```lisp
(defroute lispdoc (:get "text/*" symbol &key (doctype 'function))
  (or (documentation symbol doctype)
      (http-condition 404 "Sorry no ~a doc for ~a" doctype symbol)))
```

Actually, this will work *just fine* out of the box. But the routes
matched are not as human-readable like before: they look like this:

```
lispdoc/cl%3Adefun          ; previously lispdoc/defun
lispdoc/snooze%3Adefroute   ; previously lispdoc/defroute?package=snooze
```

Furthermore, it is conceivable that we had already published these
routes to the world, so we need to change the implementation without
changing the interface.

```lisp
(defmethod uri-to-arguments ((resource (eql #'betterdoc)) uri)
  (multiple-value-bind (plain-args keyword-args)
      (call-next-method)
    (let* ((sym-name (string (first plain-args)))
           (package-name (or (getf keyword-args :package) :cl))
           (sym (handler-case
                    (find-symbol sym-name package-name)
                  (error (e) (http-condition 400 "Malformed args (~a)" e)))))
      (unless sym
        (http-condition 404 "Sorry, no such symbol"))
      (values (cons sym (cdr plain-args))
              (loop for key in keyword-args)))))

(defmethod arguments-to-uri ((resource (eql #'betterdoc)) plain-args keyword-args)
  (let ((sym (first plain-args)))
    (call-next-method resource
                      (intern (symbol-name sym) :keyword)
                      (cons `(:package . ,(symbol-package sym))
                            keyword-args))))
```

Controlling errors
------------------

Errors and unexpected situations are part of normal HTTP life. Many
websites and REST services serve information about the conditions that
lead to an error, be it in a pretty HTML error page or a JSON object
describing the problem.

Snooze tries to make it possible to precisely control what information
gets sent to the client via a generic function and two variables:

* `explain-condition (condition resource content-type)`
* `*catch-errors*`
* `*catch-http-conditions*`

Out of the box, there no methods on `explain-condition` and the first
two variables are set to `t` by default. This means that any HTTP
condition or a Lisp error in your application will generate a very
terse reply in plain-text.

You can partially amend by writing an explain-condition that tries to
explain HTTP conditions politely in HTML:

```lisp
(defmethod explain-condition ((condition http-condition) (resource (eql #'lispdoc)) (ct snooze-types:text/html))
               (with-output-to-string (s)
                 (format s "<h1>Terribly sorry</h1><p>, but it seems you've generated a ~a</p>" (status-code condition))))
```

You use the same technique to explain *any* error like so:

```lisp
(defmethod explain-condition ((error error) (resource (eql #'lispdoc)) (ct snooze-types:text/html))
               (with-output-to-string (s)
                 (format s "<h1>Oh dear</h1><p>, but it seems I've messed here</p>")))
```

Finally, you can play around with `*catch-errors*` and
`*catch-http-conditions` (see their docstrings). I normally leave
`*catch-http-conditions*` set to `t` and `*catch-errors*` set to
either `:verbose` or `nil` depending on whether I want to do debugging
in the browser or in Emacs.

Support
-------

To discuss matters open an [issue][issues] for now or perhaps ask in
the [#lisp][sharp-lisp] IRC channel.


[quicklisp]: http://quicklisp.org
[asdf]: http://common-lisp.net/project/asdf/
[hunchentoot]: https://github.com/edicl/hunchentoot
[sharp-lisp]: irc://irc.freenode.net/#lisp
[issues]: https://github.com/capitaomorte/snooze/issues
