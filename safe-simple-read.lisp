(in-package #:snooze-safe-simple-read)

(defun parse-integer-then-float (string)
  (let (retval nread)
    (multiple-value-setq (retval nread)
      (parse-integer string :junk-allowed t))
    (cond ((/= nread (length string))
           (multiple-value-setq (retval nread)
             (parse-float:parse-float string :junk-allowed t))
           (if (/= nread (length string))
               (values nil nread)
               (values retval nread)))
          (t
           (values retval nread)))))

(defun read-string (stream &optional (terminator #\") )
  (unless (eq (read-char stream) terminator)
    (error 'reader-error
           :format-control "~a cannot read string that doesn't start with ~a"
           :format-arguments (list 'parse-string terminator)))
  (let ((nread 1))
    (values
     (handler-case
         (with-output-to-string (*standard-output*)
           (loop for c = (read-char stream) do
             (incf nread)
             (cond
               ((eq c terminator)
                (return))
               ((eq c #\\)
                (write-char (read-char stream)))
               (t
                (write-char c)))))
       (end-of-file (e)
         (error 'reader-error
                :format-control "~a sees premature end reading string inside string (~a)"
                :format-arguments (list 'parse-string e))))
     nread)))

(defun read-name (stream)
  (let* ((eof-sym (gensym))
         (c (peek-char nil stream nil eof-sym)))
    (cond ((eq c #\|)
           (read-string stream #\|))
          ((eq c eof-sym)
           (values nil 0))
          (t
           (let ((nread 0))
             (values
              (string-upcase
               (with-output-to-string (*standard-output*)
                 (loop
                   with eof-sym = (gensym)
                   for c = (read-char stream nil eof-sym) do
                     (incf nread)
                     (cond ((eq c #\:)
                            (decf nread)
                            (unread-char c stream)
                            (return))
                           ((eq c eof-sym) (decf nread) (return))
                           (t (write-char c))))))
              nread))))))

(defun parse-symbol (string)
  (let (symbol-name
        (package *package*)
        ncolons)
    (with-input-from-string (stream string)
      (multiple-value-bind (name nread)
          (read-name stream)
        (cond ((/= nread (length string))
               (setq ncolons
                     (handler-case
                         (loop for c =  (read-char stream)
                               while (eq c #\:)
                               count 1
                               finally (unread-char c stream))
                       (end-of-file (e)
                         (error 'reader-error
                                :format-control "~a sees a colon ending reading symbol from string ~a (~a)"
                                :format-arguments (list 'parse-symbol string e)))))
               (cond ((= ncolons 0)
                      (error 'reader-error
                             :format-control "~a expecting colons reading symbol from string ~a"
                             :format-arguments (list 'parse-symbol string)))
                     ((> ncolons 2)
                      (error 'reader-error
                             :format-control "~a sees too many colons reading symbol from string ~a"
                             :format-arguments (list 'parse-symbol string))))
               (setq package (or (find-package name)
                                 (error 'reader-error
                                        :format-control "~a sees does not recognize package ~a"
                                        :format-arguments (list 'parse-symbol name))))
               (multiple-value-bind (name more-nread)
                   (read-name stream)
                 (setq symbol-name name
                       nread (+ ncolons nread more-nread))
                 (when (/= nread (length string))
                   (error 'reader-error
                          :format-control "~a sees garbage reading symbol from string ~a"
                          :format-arguments (list 'parse-symbol string)))))
              (t
               (setq symbol-name name)))))
    (multiple-value-bind (symbol status)
        (find-symbol symbol-name package)
      (when (and (eq status :internal)
                 ncolons
                 (/= ncolons 2))
        (error 'reader-error
               :format-control "~a sees a symbol ~a not external in the ~a package"
               :format-arguments (list 'parse-symbol symbol-name package)))
      (values symbol status symbol-name package))))


(defun safe-simple-read-from-string (string &optional make-symbol-p)
  "Reads some objects represented by STRING.
Can only read in numbers, strings or existing symbols. Symbols may be
package-designated according to how they are written by of
WRITE-TO-STRING.

No new symbols are ever interned. A STRING value that would generate
interning of a symbol generates an error, but, if MAKE-SYMBOL-P is
non-NIL, a new uninterned symbol is returned with the name of the
would-be-interned symbol.

No macro-characters exist, not even #\(. So, where STRING would
normally represent a list to READ-FROM-STRING, it is taken as a
peculiar symbol name, that, at any rate, either exists in *PACKAGE* or
is never interned anywhere"
  (cond ((eq (aref string 0) #\")
         (with-input-from-string (stream string)
           (multiple-value-bind (obj nread)
               (read-string stream #\")
             (unless (= nread (length string))
               (error 'reader-error
                      :format-control "~a sees junk reading string inside string ~s"
                      :format-arguments (list 'safe-read-simple-token-from-string string)))
             (values obj nread))))
        (t
         (multiple-value-bind (number nread)
             (parse-integer-then-float string)
           (cond (number
                  (values number nread))
                 (t
                  (multiple-value-bind (symbol status symbol-name package)
                      (parse-symbol string)
                    (if status
                        (values symbol (length string))
                        (if make-symbol-p
                            (values (make-symbol symbol-name)
                                    (length string))
                            (error 'reader-error
                                   :format-control "~a refusing to intern a new symbol ~s in ~a"
                                   :format-arguments `(safe-simple-read-from-string
                                                       ,string
                                                       ,package)))))))))))







