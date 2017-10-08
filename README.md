[![Build Status](https://travis-ci.org/joaotavora/snooze.svg?branch=master)](https://travis-ci.org/joaotavora/snooze)
Snooze
======

_Snooze_ is an URL router for Common Lisp designed around [REST web
services][rest].

An URL router lets you open URL routes to your application that are
friendlier, easier to remember and better supported by other
applications, such as search engines. RESTful routes are near universal
in web APIs and [look like this][restful-routes].

All _Snooze_ does is establish a tight fit between this type of 
route and plain old Common Lisp. For example, in _Snooze_, routes 
are just functions and HTTP conditions are just Lisp conditions.

Since you stay inside Lisp, if you know how to make a function,
you know how to make a route. *There are no regular expressions to
write or extra route-defining syntax to learn*.

_Snooze_ is web-server-backend-agnostic: it can
[work with any web server](#other-backends).

Here's an example you can try out quickly: a micro-REST service to
read and write Lisp docstrings over HTTP:

```lisp
(defpackage #:readme-demo (:use #:cl #:snooze))
(in-package #:readme-demo)

(defun find-symbol-or-lose (name package)
  (or (find-symbol (string name) (find-package package))
      (http-condition 404 "Sorry, no such symbol")))

(defroute lispdoc (:get :text/* name &key (package :cl) (doctype 'function))
  (or (documentation (find-symbol-or-lose name package) doctype)
      (http-condition 404 "Sorry, ~a doesn't have any ~a doc" name doctype)))

(defroute lispdoc (:post :text/plain name &key (package :cl) (doctype 'function))
  (setf (documentation (find-symbol-or-lose name package) doctype)
        (payload-as-string)))

;; Let's use clack as a server backend
(clack:clackup (snooze:make-clack-app) :port 9003)
```

This establishes two routes (`GET` for reading and `POST` for writing)
on the URI `localhost:9003/lispdoc/<symbol>`. Here's an illustration
of how they respond:


```HTTP
GET /lispdoc/defun                         => 200 OK
GET /lispdoc/funny-syntax?package=snooze   => 404 Not found
GET /lispdoc/in/?valid=args                => 400 Bad Request
                                           
GET /lispdoc/defun                         => 406 Not Acceptable 
Accept: application/json

POST /lispdoc/scan?package=cl-ppcre        => 200 OK 
Content-type: text/plain

POST /lispdoc/defun                        => 415 Unsupported Media Type 
Content-type: application/json
```

The error codes 400, 406 and 415 are error reporting that you get "for
free": if the HTTP client strays off these routes, be it for improper
syntax or unsupported content types, the correct HTTP condition is
signalled.

The rest of this README contains the [rationale](#rationale) for
building _Snooze_ and a [tutorial](#tutorial) that builds on the
simple application presented above.

Status
------

Ah, _Snooze_ is kinda *BETA*. The usual disclaimer of warranty
applies.

Rationale
---------

There are already some Common Lisp systems for HTTP routing, like
[caveman][caveman], [cl-rest-server][cl-rest-server],[restas][restas]
and [ningle][ningle]. Unfortunately, they tend to make you learn some
extra route-defining syntax. 

On the contrary _Snooze_ maps
[REST/HTTP](https://en.wikipedia.org/wiki/REST) concepts to Common
Lisp concepts:

| HTTP/REST concept                    | Snooze CL concept                   |
| :----------------------------------- | ----------------------------------: |
| REST resource                        | CLOS generic function               |
| Route                                | CLOS method                         |
| Verbs (`GET`, `POST`, `DELETE`, etc) | CLOS specializer on first argument  |
| `Accept:` and `Content-Type:`        | CLOS specializer on second argument |
| URI path (`/path1/path2/path3)`)     | Required and optional arguments     |
| URL queries (`?param=value&p2=v2`)   | Keyword arguments                   |
| Status codes (`404`, `500`, etc)     | CL conditions                       |

This has many advantages, for example

* since every route is a method, you can `trace` it like a regular
  function, find its definition with `M-.` or even use `:around`
  qualifiers;
* using a regular lambda-list guarantees that URI errors can be
  spotted early by your compiler;
* there is no need to write code to "extract" arguments from the
  URI. 
* Since _Snooze_ knows the lambda-list of a route, it can use it
  to do the reverse of URI matching: generate URIs that perfectly
  match that same route.


Tutorial
--------

Consider the code sample presented [above](#snooze). Let's pick up
where we left off, and build a bit more of `lispdoc`, our
docstring-manipulating application. We'll see how to:

* [understand _Snooze_'s REST resources](#resources-as-generic-functions)
* [dispatch on HTTP methods and content-types](#content-types)
* [generate and encode compatible URIs](#uri-generation)
* [grafully handle failure conditions](#controlling-errors)
* [control conversion of URI arguments](#how-snooze-converts-uri-components-to-arguments)
* [refactor routes without changing the API](#tighter-routes)
* [hook _Snooze_ into the backend of your choice](#other-backends)

This tutorial assumes you're using a recent version of
[quicklisp][quicklisp] so start by entering this into your REPL.

```lisp
(push "path/to/snoozes/parent/dir" quicklisp:*local-project-directories*)
(ql:quickload :snooze)
```

Make sure you keep an eye on the docstrings of the functions
mentioned, *they are where the real API reference lives*. Find them
all, appropriately, in the
[api.lisp](https://github.com/joaotavora/snooze/blob/master/api.lisp)
file.

### Resources as generic functions

An important detail that was elided from the initial sample is that,
in _Snooze_:

* a REST _resource_ is implemented a CLOS generic
function.
* The _operations_ (`GET`, `POST`, `DELETE`, etc...) accepted by a
resource are implemented as CLOS methods on that generic function

When a HTTP request is received, _Snooze_ arranges for its URI to be
translated into a generic function name and its remaining properties
(verb, content-type, additional URI bits) to be translated into
arguments for that function. _Snooze_ then calls that function with
those arguments and CLOS does the rest:

* if one of the methods of this generic function matches, its body is
called and the HTTP client sees a nice response;

* otherwise a condition is signalled and _Snooze_ takes care that the
HTTP client sees the correct error code.



Under the hood, `defroute` is actually a really thin wrapper on
`defmethod`. You can even use `defmethod` directly if you prefer:

```lisp
(defmethod lispdoc
            ((snooze-verbs:http-verb snooze-verbs:post)
             (snooze-types:type snooze-types:text/plain) name
             &key (package :cl) (doctype 'function))
   (setf (documentation (find-symbol-or-lose name package) doctype)
         (payload-as-string)))
```

Likewise there is a `defresource` form that is equivalent to
`defgeneric`. It may be left out since it is implicit in the first
`defroute` call.

This means we could have defined the above application in an
equivalent terser form:

```lisp
(defresource lispdoc (verb content-type name &key)
  (:route (:get :text/* name &key (package :cl) (doctype 'function))
    (or (documentation (find-symbol-or-lose name package) doctype)
        (http-condition 404 "Sorry, ~a doesn't have any ~a doc" name doctype)))
  (:route (:post :text/plain name &key (package :cl) (doctype 'function))
    (setf (documentation (find-symbol-or-lose name package) doctype)
          (payload-as-string))))
```

### Content-Types

Let's start by serving docstrings in HTML. As seen above, we already
have a route which serves plain text:

```lisp
(defroute lispdoc (:get :text/* name &key (package :cl) (type 'function))
  (or (documentation (find-symbol-or-lose name package) type)
      (http-condition 404 "Sorry no ~a doc for ~a" type name)))
```

To add a similar route for the content-type `text/html`, we just
notice that `text/html` *is* `text/*`. Also because routes are really
only CLOS methods, the easiest way is:

```lisp
(defroute lispdoc :around (:get :text/html name &key &allow-other-keys)
  (format nil "<h1>Docstring for ~a</h1><p>~a</p>"
          name (call-next-method)))
```

This will do fine for now. Of course, later we should probably escape
the HTML with something like [cl-who][cl-who]'s
`escape-string-all`. We might also consider removing the `:around`
qualifier and use a helper function shared by two routes.

Let's try our hand at implementing an important part of the API:
`POST` requests with JSON content:

```lisp
(defroute lispdoc (:post "application/json" name &key (package :cl) (doctype 'function))
  (let* ((json (handler-case
                   ;; you'll need to quickload :cl-json
                   (json:decode-json-from-string
                    (payload-as-string))
                 (error (e)
                   (http-condition 400 "Malformed JSON (~a)!" e))))
         (sym (find-symbol-or-lose name package))
         (docstring (cdr (assoc :docstring json))))
    (if (and sym docstring doctype)
        (setf (documentation sym doctype) docstring)
        (http-condition 400 "JSON missing some properties"))))
```

### URI generation

Our application has a growing number of routes that work fine,
provided the use knows how to type them.  Because this is increasingly
complicated as more and more routes are added, it is very often the
case that we'll want parts of the our REST application to generate
URI's that match its own routes. Probably, the most common case is
providing a link to a specific resource in an HTML response.

This is very easy to do in _Snooze_, as it can automatically generate
the URIs for a resource. You first need to get a "genpath" function
for your resource. Just do:


```lisp
(defgenpath lispdoc lispdoc-path)
```

Or, alternatively, pass `:genpath` to `defresource`.

```
(defresource lispdoc (verb ct symbol) (:genpath lispdoc-path))
```

The newly generated `lispdoc-path` has an argument list that perfectly
matches your route's arguments:

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
correct URL when an easily-fixed 404 happens:

```lisp
(defun doc-not-found-message (name package doctype)
  (let* ((othertype (if (eq doctype 'function) 'variable 'function))
         (otherdoc (documentation (find-symbol-or-lose name package) othertype)))
    (with-output-to-string (s)
      (format s "There is no ~a doc for ~a." doctype name)
      (when otherdoc
        (format s "<p>But try <a href=~a>here</a></p>"
                (lispdoc-path name :package package :doctype othertype))))))

(defroute lispdoc (:get :text/html name &key (package :cl) (doctype 'function))
  (or (documentation (find-symbol-or-lose name package) doctype)
      (http-condition 404 (doc-not-found-message name package doctype))))
```

If you now point your browser to:

```
http://localhost:9003/lispdoc/%2Astandard-output%2A?doctype=variable
```

You should see a nicer 404 error message. **Except you don't (!)**,
because, by default, _Snooze_ is very terse with error messages and we
haven't told it not to be. So don't worry, the next sections explains
how to change that.

Controlling errors
------------------

Errors and unexpected situations are part of normal HTTP life. Many
websites and REST services not only return an HTTP status code, but
also serve information about the conditions that lead to an error, be
it in a pretty HTML error page or a JSON object describing the
problem.

_Snooze_ tries to make it possible to precisely control what
information gets sent to the client. It uses a generic function and
two variables:

* `explain-condition (condition resource content-type)`
* `*catch-errors*`
* `*catch-http-conditions*`

Out of the box, there are no methods on `explain-condition` and the
two variables are set to `t`.

This means that any HTTP condition or a Lisp error in your application
will generate a very terse reply in plain-text containing only the
status code and the standard reason phrase.

You can amend this selectively by adding`explain-condition`
methods that explain HTTP conditions politely in, say, HTML:

```lisp
(defmethod explain-condition ((condition http-condition)
                              (resource (eql #'lispdoc))
                              (ct snooze-types:text/html))
               (with-output-to-string (s)
                 (format s "<h1>Terribly sorry</h1><p>You might have made a mistake, I'm afraid</p>")
                 (format s "<p>~a</p>" condition)))
```

The above explains only HTTP conditions that are the client's fault, but you can use the same technique to explain *any* error, like so:

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

How Snooze converts URI components to arguments
-----------------------------------------------

You might have noticed already that the arguments passed to the CLOS
generic functions that represent resources are actual Lisp symbols
extracted from the URI, whereas other frameworks normally pass them as
strings.

What are the advantages of this? Let's drift from the `lispdoc`
example a bit. Consider this fragment of a Beatle-listing app.


```lisp
(defclass beatle () ((id      :initarg :id)
                     (name    :initarg :name    :accessor name)
                     (guitars :initarg :guitars :accessor number-of-guitars)))

(defparameter *beatles*
           (list (make-instance 'beatle :id 1 :name "John" :guitars 1)
                 (make-instance 'beatle :id 2 :name "Paul" :guitars 2)
                 (make-instance 'beatle :id 3 :name "Ringo" :guitars 0)
                 (make-instance 'beatle :id 4 :name "George" :guitars 10)))

(defroute beatles (:get "text/plain" &key (key 'number-of-guitars) (predicate '>))
  (assert-safe-functions key predicate)
  (format nil "~{~a~^~%~}"
          (mapcar #'name
                  (sort (copy-list *beatles*) predicate :key key))))

(defgenpath beatles beatles-path)
```

The `defgenpath` form makes `beatles-path` be a function of two
keyword arguments, `:key` and `:predicate` that returns the perfect
URI for accessing the `beatles` route. Among other things you can name
regular functions (like `<` and `string-lessp` in this example) by
their symbols, as you would in pure Lisp.

```lisp
CL-USER> (beatles-path :key 'number-of-guitars :predicate '<)
"/beatles/?key=number-of-guitars&predicate=%3C"
CL-USER> (beatles-path :key 'name :predicate 'string-lessp)
"/beatles/?key=name&predicate=string-lessp"
```

Sure enough, feeding these URIs to the HTTP client causes the function
`beatles` to be called with exactly the same symbols that you passed
to `beatles-path`.

Now, if you're thinking that this doesn't fit needs, know that it is
merely a default behaviour, and entirely configurable: if you really
want to have the URI path `foo/bar/baz` become the strings `"foo"`,
`"bar"` and `"baz"` in your application you merely need to add a CLOS
method to the each of the generic functions `read-for-resource` and
`write-for-resource`.

Nevertheless, I recommend you keep the default:

* The default `read-for-resource` uses a very locked down version of
   `cl:read-to-string` that doesn't intern symbols (for security),
   allow any kind of reader macros or read anything more complicated
   than a number, a string or a symbol.

* The default `write-for-resource` does the inverse: it writes onto a
  string of any object so that `read-for-resource` can reconstruct
  that object from the string (so long as the object is a secure thing
  to serialize over URI).

There is perhaps a better way to influence the mapping between URIs
and arguments. To that effect, two other functions are discussed in
the next section: `arguments-to-uri` and `uri-to-arguments`.

Tighter routes
--------------

Let's recall the `lispdoc` app. The routes we have until now are
functions of a string. To convert them into actual symbols they need:

* the `find-symbol-or-lose` helper;
* an additional `:package` keyword arg.

This isn't pretty: it would be nicer if routes were functions of a
symbol. After all, in Common Lisp, passing symbols around shouldn't
force you to pass their packages separately!

So basically, we want to write our methods like this:

```lisp
(defroute lispdoc (:get :text/* (sym symbol) &key (doctype 'function))
  (or (documentation sym doctype)
      (http-condition 404 (doc-not-found-message sym doctype))))
```

Actually, this will work *just fine* out of the box. Oh wait, now our routes
look like this:

```lisp
(lispdoc-path 'cl-ppcre:scan)
  ;; => "/lispdoc/cl-ppcre%3Ascan"
(lispdoc-path 'ql:quickload)
  ;; => "/lispdoc/quicklisp-client%3Aquickload"
```

Compare these to the routes at the very top of this document:

```lisp
  /lispdoc/scan?package=cl-ppcre
  /lispdoc/quickload?package=ql
```

You might be dissapointed that the new ones are not as human-readable
(the `%3A` encoding for the `:` looks would look slightly bizarre to a
user reading it in the browser's address bar). But even if you don't
care about appearance and find them perfectly acceptable, it is
conceivable that we had already published the routes of the older REST
API to the world.

So, to keep that API, we need to change the implementation without
changing the interface. This is where `uri-to-arguments` and its
reciprocal `arguments-to-uri` come in handy.  These generic functions
have default implementations for all resources that can be leveraged
for surgical tweaks like the one we need here:

* `uri-to-arguments` receives an URI string, and make it compute a
  values-list of "plain" and keyword arguments that are passed to the
  route (after the verb and content type). In our case, we first parse
  the plain symbol name and `:package` using `call-next-method`, then
  compute an actual symbol. We also make sure to not pass `:package`
  to our new route, as it doesn't accept it.

* `arguments-to-uri` is the function that allows the `genpath`
  function to produce matching URIs. It does the reverse, producing an
  URI string.  In this case it takes a symbol in `plain-args`, After
  extracting its package and massaging it into an uninterned symbol,
  we also use `call-next-method` to simplify things.

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
                      (cons `(:package . ,(make-symbol
                                           (package-name (symbol-package sym))))
                            keyword-args))))
```

We can now safely rewrite the remaining routes in much simpler
fashion. Here's the rest of the application now (notice also how
`doc-not-found-message` was also simplified)

```lisp
(defun doc-not-found-message (symbol doctype)
  (let* ((othertype (if (eq doctype 'function) 'variable 'function))
         (otherdoc (documentation symbol othertype)))
    (with-output-to-string (s)
      (format s "There is no ~a doc for ~a." doctype symbol)
      (when otherdoc
        (format s "<p>But try <a href=~a>here</a></p>"
                (lispdoc-path symbol :doctype othertype))))))

(defroute lispdoc (:get :text/* (sym symbol) &key (doctype 'function))
  (or (documentation sym doctype)
      (http-condition 404 "No doc found for ~a" sym)))

(defroute lispdoc (:post :text/plain (sym symbol) &key (doctype 'function))
  (setf (documentation sym doctype)
        (payload-as-string)))

(defroute lispdoc (:get :text/html (sym symbol) &key (doctype 'function))
  (or (documentation sym doctype)
      (http-condition 404 (doc-not-found-message sym doctype))))

(defroute lispdoc (:post :application/json (sym symbol) &key (doctype 'function))
  (let* ((json (handler-case
                   (json:decode-json-from-string
                    (payload-as-string))
                 (error (e)
                   (http-condition 400 "Malformed JSON! (~a)" e))))
         (docstring (cdr (assoc :docstring json))))
    (setf (documentation sym doctype) docstring)))
```

Other backends
--------------

_Snooze_ is web-server agnostic: it's just an URL router.  It does
come with two utility functions, `make-clack-app` and
`make-hunchentoot-app` that will plug into two popular web servers and
quickly let you jump into the action:

```lisp
;;; Use hunchentoot directly
(push (snooze:make-hunchentoot-app) hunchentoot:*dispatch-table*)
(hunchentoot:start (make-instance 'hunchentoot:easy-acceptor :port 9003))

;;; Use clack
(clack:clackup (snooze:make-clack-app) :port 9003)
```

But Snooze doesn't "require" Clack or Hunchentoot in any sense. So if
you want to use any other backend, I suggest you take a look at the
implementations of `make-hunchentoot-app` and `make-clack-app`
functions, particularly their use of `snooze:handle-request`.

Support
-------

To ask questions, report bugs, or just discuss matters open an
[issue][issues] or send me email.

[quicklisp]: http://quicklisp.org
[asdf]: http://common-lisp.net/project/asdf/
[hunchentoot]: https://github.com/edicl/hunchentoot
[sharp-lisp]: irc://irc.freenode.net/#lisp
[issues]: https://github.com/joaotavora/snooze/issues
[caveman]: https://github.com/fukamachi/caveman#routing
[clack]: https://github.com/fukamachi/clack
[cl-rest-server]: https://github.com/mmontone/cl-rest-server
[restas]: http://restas.lisper.ru/en/manual/routes.html#routes
[ningle]: http://8arrow.org/ningle/
[cl-who]: http://weitz.de/cl-who/#escape-string-all
[rest]: https://en.wikipedia.org/wiki/Representational_state_transfer
[restful-routes]: https://en.wikipedia.org/wiki/Representational_state_transfer#Relationship%20between%20URL%20and%HTTP%20methods
