(in-package :cl-user) 

(uiop:define-package :signal-translator/markup-lexer
    (:nicknames :markup-lexer)
  (:use :common-lisp :cl-markup)
  (:use :signal-translator/lexical-analyzer
	:signal-translator/test-lexer)
  (:export #:markup-lexer-output
	   #:markup-data-rows
	   #:markup-table
	   #:markup-hash-table
	   #:markup-error-table
	   #:markup-paragraph))

(in-package :markup-lexer)

(defun markup-head (head-row)
  (cons :tr
	(mapcar (lambda (h) (list :th (format nil "~a" h)))
		head-row)))

(defun markup-row (row)
  (cons :tr
	(mapcar (lambda (d) (list :td (format nil "~a" d)))
		row)))

(defun markup-data-rows (data-rows)
  (mapcar #'markup-row data-rows))

(defun markup-table (head-row data-rows)
  (cons :table
	(cons (markup-head head-row)
	      (markup-data-rows data-rows))))

(defun markup-token-table (token-table reversed-kw-table
			   reversed-id-table constants-table)
  (markup-table '("line" "position" "token value" "token representation")
		(map 'list
		     (lambda (token)
		       (list (first (first token))
			     (second (first token))
			     (second token)
			     (dump-token (second token) reversed-kw-table
					 reversed-id-table constants-table)))
		     token-table)))

(defun hash-table-to-alist (ht)
  (let ((alist nil))
    (maphash (lambda (k v) (push (list k v) alist))
	     ht)
    (reverse alist)))

(defun markup-hash-table (hash-table)
  (markup-table '("key" "value")
		(hash-table-to-alist hash-table)))

(defun markup-constant-table (constants-table)
  (markup-table '("key" "value")
		(mapcar (lambda (x)
			  (list (first x)
				(dump-constant (second x))))
			(hash-table-to-alist constants-table))))

(defun markup-paragraph (string)
  (list :p string))

(defun markup-error-table (error-table who)
  (print (markup-table
	  nil
	  (map 'list (lambda (error-message)
		       (format nil "~a: ~a~%" who error-message))
	       error-table))))

(defun markup-lexer-output (lexer-output)
  (destructuring-bind ((token-table
			(reversed-id-table identifiers-table)
			constants-table)
		       (reversed-kw-table keywords-table)
		       error-table)
      lexer-output
    (declare (ignore identifiers-table keywords-table))
    (markup* (markup-paragraph "TOKEN TABLE:")
	     (markup-token-table token-table reversed-kw-table
				 reversed-id-table constants-table)
	     (markup-paragraph "KEYWORD TABLE:")
	     (markup-hash-table reversed-kw-table)
	     (markup-paragraph "IDENTIFIER TABLE:")
	     (markup-hash-table reversed-id-table)
	     (markup-paragraph "CONSTANT TABLE:")
	     (markup-constant-table constants-table)
	     (markup-paragraph "ERRORS:")
	     (markup-error-table error-table :lexer))))
