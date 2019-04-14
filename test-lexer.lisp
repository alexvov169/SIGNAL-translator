(in-package :cl-user) 

(uiop:define-package :signal-translator/test-lexer
    (:nicknames :test-lexer)
  (:use :common-lisp :signal-translator/lexical-analyzer)
  (:export #:call-lexer))

(in-package :test-lexer)

(defun reverse-hash-table (hash-table &key (new-test #'eql))
  (let ((result (make-hash-table :test new-test)))
    (maphash (lambda (key value)
               (setf (gethash value result)
                     key))
             hash-table)
    result))

(defun dump-token (token-value reversed-kw-table
                   reversed-id-table constants-table
                   identifier-first constant-first)
  (cond ((< token-value 256) (code-char token-value))
        ((< token-value identifier-first) (gethash token-value reversed-kw-table))
        ((< token-value constant-first) (gethash token-value reversed-id-table))
        (t (dump-constant (gethash token-value constants-table)))))

(defun print-token-table (token-table reversed-kw-table
                          reversed-id-table constants-table
                          identifier-first constant-first outstream)
  (map 'vector (lambda (token)
                (format outstream "~5a~5a~10a~30a~%"
                        (first (first token))
                        (second (first token))
                        (second token)
                        (dump-token (second token)
                                    reversed-kw-table
                                    reversed-id-table constants-table
                                    identifier-first constant-first)))
       token-table))

(defun print-hash-table (printer hash-table &optional outstream)
  (maphash (lambda (key value)
             (funcall printer key value outstream))
           hash-table))

(defun print-identifier-tuple (key value &optional outstream)
  (format outstream "~10a:  ~30a~%" key value))

(defun dump-constant (value &optional outstream)
  (if (numberp (second value))
      (format outstream "~a + i*~a" (second (first value))
              (second value))
      (format outstream "~a * e^(i*~a)" (second (first value))
              (second (second value)))))

(defun print-constant-tuple (key value &optional outstream)
  (format outstream "~10a:  ~30a~%" key (dump-constant value)))

(defun call-lexer (filespec outstream)
  (let ((identifier-first 65536)
        (constant-first 1048576))
    (multiple-value-bind (tables keywords-table error-table)
        (lexer filespec identifier-first constant-first)
      (destructuring-bind (token-table identifiers-table constants-table)
          tables
        (let ((reversed-id-table (reverse-hash-table identifiers-table))
              (reversed-kw-table (reverse-hash-table keywords-table)))
          (format outstream "~%TOKEN TABLE:~%")
          (print-token-table token-table reversed-kw-table
                             reversed-id-table constants-table
                             identifier-first constant-first outstream)
          (format outstream "~%KEYWORD TABLE:~%")
          (print-hash-table #'print-identifier-tuple reversed-kw-table outstream)
          (format outstream "~%IDENTIFIER TABLE:~%")
          (print-hash-table #'print-identifier-tuple reversed-id-table outstream)
          (format outstream "~%CONSTANT TABLE:~%")
          (print-hash-table #'print-constant-tuple constants-table outstream)))
      (format outstream "~%ERRORS:~%")
      (map 'vector (lambda (error-message)
                    (format outstream "LEXER> ~a~%"
                            error-message))
           error-table)))
  t)
