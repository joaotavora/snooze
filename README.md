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

_Snooze_ can generate the URIs for a resource. To get a function that
does that do:

```lisp
(defgenpath lispdoc lispdoc-path)
```

The generated function has an arglist matching your route's arguments:

```lisp
(lispdoc-path 'defroute :package 'snooze)
  ;; => "/lispdoc/defroute?package=snooze"
(lispdoc-path 'defun)
  ;; => "/lispdoc/defun"
(lispdoc-path '*standard-output* :doctype 'variable)
  ;; => "/lispdoc/%2Astandard-output%2A?doctype=variable"
(lispdoc-path '*standard-output* :FOO 'hey)
  ;; error! unknown &KEY argument: :FOO
```

Notice the automatic URI-encoding of the `*` character and how the
function errors on invalid keyword arguments that would produce an
invalid route.

Path generators are useful, for example, when write HTML links to your
resources. In our example, let's use it to guide the user to the
correct URL when a 404 happens:

```lisp
(defun doc-not-found-message (name package doctype)
  (let* ((othertype (if (eq doctype 'function) 'variable 'function))
         (otherdoc (documentation (find-symbol-or-lose name package) othertype)))
    (with-output-to-string (s)
      (format s "Sorry no ~a doc for ~a." doctype name)
      (when otherdoc
        (format s "~&But try <a href=~a>here</a>"
                (lispdoc-path name :package package :doctype othertype))))))

(defroute lispdoc (:get "text/html" name &key (package :cl) (doctype 'function))
  (or (documentation (find-symbol-or-lose name package) doctype)
      (http-condition 404 (doc-not-found-message name package doctype))))
```

If you now point your browser to:

```
http://localhost:9003/lispdoc/%2Astandard-output%2A?doctype=variable
```

You should see a nicer 404 error message. Except you don't, because by
default _Snooze_ is very terse with error messages and we haven't told it
not to be. The next sections explains how to change that.

Controlling errors
------------------

Errors and unexpected situations are part of normal HTTP life. Many
websites and REST services not only return an HTTP status code, but
also serve information about the conditions that lead to an error, be
it in a pretty HTML error page or a JSON object describing the
problem.

Snooze tries to make it possible to precisely control what information
gets sent to the client. It uses a generic function and two variables:

* `explain-condition (condition resource content-type)`
* `*catch-errors*`
* `*catch-http-conditions*`

Out of the box, there are no methods on `explain-condition` and the
first two variables are set to `t` by default.

This means that any HTTP condition or a Lisp error in your application
will generate a very terse reply in plain-text containing only the
status code and the standard reason phrase.

You can amend this selectively by writing an `explain-condition`
methods explain HTTP conditions politely in HTML:

```lisp
(defmethod explain-condition ((condition http-condition)
                              (resource (eql #'lispdoc))
                              (ct snooze-types:text/html))
               (with-output-to-string (s)
                 (format s "<h1>Terribly sorry</h1><p>You might have made a mistake, I'm afraid</p>")
                 (format s "<p>~a</p>" condition)))
```

You can use the same technique to explain *any* error like so:

```lisp
(defmethod explain-condition ((error error) (resource (eql #'lispdoc)) (ct snooze-types:text/html))
               (with-output-to-string (s)
                 (format s "<h1>Oh dear</h1><p>It seems I've messed up somehow</p>")))
```

Finally, you can play around with `*catch-errors*` and
`*catch-http-conditions` (see their docstrings). I normally leave
`*catch-http-conditions*` set to `t` and `*catch-errors*` set to
either `:verbose` or `nil` depending on whether I want to do debugging
in the browser or in Emacs.

Tighter routes
---------------

The routes we have until now all use:

* the `find-symbol-or-lose` helper;
* the `:package` keyword arg.

It would be nicer if they were simply functions of a symbol: After all
, in Common Lisp, passing symbols around doesn't force you to pass
their packages separately!

So basically, we want to write routes like this:

```lisp
(defroute lispdoc (:get "text/*" symbol &key (doctype 'function))
  (or (documentation symbol doctype)
      (http-condition 404 "Sorry no ~a doc for ~a" doctype symbol)))
```

Actually, this will work *just fine* out of the box. But the routes
matched are not as human-readable like before: they look like this:

```
(lispdoc-path 'cl-ppcre:scan)
  ;; => "/lispdoc/cl-ppcre%3Ascan"
(lispdoc-path 'ql:quickload)
  ;; => "/lispdoc/quicklisp-client%3Aquickload"
```

Even if you find that perfectly acceptable, it is conceivable that we
had already the other kind of routes to the world, so we need to
change the implementation without changing the interface. This is
where `uri-to-arguments` and `arguments-to-uri` might help:


```lisp
(defmethod uri-to-arguments ((resource (eql #'lispdoc)) uri)
  (multiple-value-bind (plain-args keyword-args)
      (call-next-method)
    (let* ((sym-name (string (first plain-args)))
           (package-name (or (cdr (assoc :package keyword-args)) 'cl))
           (sym (find-symbol sym-name package-name)))
      (unless sym
        (http-condition 404 "Sorry, no such symbol"))
      (values (cons sym (cdr plain-args))
              (remove :package keyword-args :key #'car)))))

(defmethod arguments-to-uri ((resource (eql #'lispdoc)) plain-args keyword-args)
  (let ((sym (first plain-args)))
    (call-next-method resource
                      (list sym)
                      (cons `(:package . ,(read-for-resource
                                           resource
                                           (package-name (symbol-package sym))))
                            keyword-args))))
```



Support
-------

To discuss matters open an [issue][issues] for now or perhaps ask in
the [#lisp][sharp-lisp] IRC channel.


[quicklisp]: http://quicklisp.org
[asdf]: http://common-lisp.net/project/asdf/
[hunchentoot]: https://github.com/edicl/hunchentoot
[sharp-lisp]: irc://irc.freenode.net/#lisp
[issues]: https://github.com/capitaomorte/snooze/issues
