;;; Helpers for backends
;;;
;;; "For backends" means that #:snooze should not use any of these
;;;
(in-package #:snooze-utils)

(defun parse-args-in-uri (args-string query-string)
  (let* ((query-and-fragment (scan-to-strings* "(?:([^#]+))?(?:#(.*))?$"
                                                      query-string))
         (required-args (cl-ppcre:split "/" (subseq args-string (mismatch "/" args-string))))
         (keyword-args (loop for maybe-pair in (cl-ppcre:split "[;&]" (first query-and-fragment))
                             for (key-name value) = (scan-to-strings* "(.*)=(.*)" maybe-pair)
                             when (and key-name value)
                               append (list (intern (string-upcase key-name) :keyword)
                                            value)))
         (fragment (second query-and-fragment)))
    (append required-args
            keyword-args
            (when fragment
              (list 'snooze:fragment fragment)))))

(defun find-resource-by-name (name server)
  (loop for package in (snooze:route-packages server)
        for sym = (find-symbol (string-upcase name) package)
          thereis (and (fboundp sym)
                       (symbol-function sym))))

(defun parse-uri (script-name query-string server)
  "Parse URI for SERVER. Return values RESOURCE ARGS CONTENT-TYPE."
  ;; <scheme name> : <hierarchical part> [ ? <query> ] [ # <fragment> ]
  ;;
  (let* ((resource-name-regexp (snooze:resource-name-regexp server))
         (match (multiple-value-list (cl-ppcre:scan resource-name-regexp
                                                    script-name)))
         (resource-name
           (and (first match)
                (apply #'subseq script-name
                       (if (plusp (length (third match)))
                           (list (aref (third match) 0) (aref (fourth match) 0))
                           (list (first match) (second match))))))
         (first-slash-resource (find-resource-by-name resource-name server))
         (resource (or (and resource-name first-slash-resource)
                       (let ((home (snooze:home-resource server)))
                         (cond ((functionp home)
                                home)
                               ((symbolp home)
                                (fboundp home))
                               ((stringp home)
                                (find-resource-by-name home server))))))
         (script-minus-resource (if first-slash-resource
                                    (subseq script-name (second match))
                                    script-name))
         (extension-match (cl-ppcre:scan "\\.\\w+$" script-minus-resource))
         (args-string (if extension-match
                          (subseq script-minus-resource 0 extension-match)
                          script-minus-resource))
         (extension (if extension-match
                        (subseq script-minus-resource (1+ extension-match))))
         (content-type-class (and extension
                                  (find-content-class
                                   (gethash extension *mime-type-hash*))))
         (actual-arguments (parse-args-in-uri (if content-type-class
                                                  args-string
                                                  (if (zerop (length args-string))
                                                      ""
                                                      script-minus-resource))
                                              query-string)))
    (values resource
            actual-arguments
            content-type-class)))

(defun parse-accept-header (string server resource)
  "Return a list of class objects designating " 
  (loop for media-range-and-params in (cl-ppcre:split "\\s*,\\s*" string)
        for media-range = (first (scan-to-strings* "([^;]*)"
                                                   media-range-and-params))
        for class = (find-content-class media-range)
        when class
          append (snooze:expand-content-type server resource class)))

(defun arglist-compatible-p (resource args)
  (handler-case
      (progn
        (check-arguments resource (append
                                          (list 'dummy 'dummy)
                                          args))
        t)
    (error () nil)))

(defun parse-content-type-header (string)
  "Return a symbol designating a SNOOZE-SEND-TYPE object."
  (find-content-class string))

(defun find-verb-or-lose (designator)
  (let ((class (or (probe-class-sym
                    (intern (string-upcase designator)
                            :snooze-verbs))
                   (error "Can't find HTTP verb for designator ~a!" designator))))
    ;; FIXME: perhaps use singletons here
    (make-instance class)))
