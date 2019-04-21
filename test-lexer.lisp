(in-package :cl-user) 

(uiop:define-package :signal-translator/test-lexer
    (:nicknames :test-lexer)
  (:use :common-lisp :signal-translator/lexical-analyzer)
  (:export #:call-lexer
	   #:print-error-table
	   #:dump-token
	   #:dump-constant))

(in-package :test-lexer)

(defun dump-token (token reversed-kw-table
                   reversed-id-table constants-table
		   &aux (token-value (second token)))
  (case (first token)
    (:single-char (code-char token-value))
    (:keyword (gethash token-value reversed-kw-table))
    (:identifier (gethash token-value reversed-id-table))
    (:complex-constant (dump-constant (gethash token-value constants-table)))
    (t (error "Lexer dump error: wrong token"))))

(defun print-token-table (token-table reversed-kw-table
                          reversed-id-table constants-table outstream)
  (map 'vector (lambda (token)
                (format outstream "~5a~5a~20a~30a~%"
                        (first (first token))
                        (second (first token))
                        (second token)
                        (dump-token (second token)
                                    reversed-kw-table
                                    reversed-id-table constants-table)))
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

(defun print-error-table (error-table who outstream)
  (map 'vector (lambda (error-message)
		 (format outstream "~a: ~a~%" who error-message))
       error-table))

(defun call-lexer (filespec outstream)
  (destructuring-bind ((token-table
			(reversed-id-table identifiers-table)
			constants-table)
		       (reversed-kw-table keywords-table)
		       error-table)
      (lexer filespec)
    (declare (ignore identifiers-table keywords-table))
    (format outstream "~%TOKEN TABLE:~%")
    (print-token-table token-table reversed-kw-table
		       reversed-id-table constants-table outstream)
    (format outstream "~%KEYWORD TABLE:~%")
    (print-hash-table #'print-identifier-tuple reversed-kw-table outstream)
    (format outstream "~%IDENTIFIER TABLE:~%")
    (print-hash-table #'print-identifier-tuple reversed-id-table outstream)
    (format outstream "~%CONSTANT TABLE:~%")
    (print-hash-table #'print-constant-tuple constants-table outstream)
    (format outstream "~%ERRORS:~%")
    (print-error-table error-table "LEXER" outstream))
  t)
