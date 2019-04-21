(in-package :cl-user) 

(uiop:define-package :signal-translator/markup-parser
    (:nicknames :markup-parser)
  (:use :common-lisp :cl-markup)
  (:use :signal-translator/lexical-analyzer
	:signal-translator/test-lexer
        :signal-translator/markup-lexer
	:signal-translator/syntax-analyzer
	:signal-translator/test-parser)
  (:export #:markup-parser-output))

(in-package :markup-parser)

(defun markup-tree (tree reversed-kw-table reversed-id-table constants-table)
  (labels ((%dump-token (token-type token-value)
	     (dump-token (list token-type token-value) reversed-kw-table
			 reversed-id-table constants-table))
	   (%aux (root branches)
	     (list :li (list :span (vertex-labeller root))
		   (cons :ul
			 (mapcar
			  (lambda (branch)
			    (if (atom branch)
				(list :li
				      (list :span
					    (if (eq :empty branch)
						(vertex-labeller root)
						(vertex-labeller
						 (%dump-token root branch)))))
				(%aux (first branch) (rest branch))))
			  branches)))))
    (list :ul (%aux (first tree) (rest tree)))))

(defun markup-parser-output (lexer-output)
  (destructuring-bind (tree reversed-kw-table reversed-id-table
		       constants-table parser-error-table)
      (parser lexer-output)
    (markup* `(:div ,(print (markup-tree tree reversed-kw-table
					 reversed-id-table constants-table)))
	     `(:div (raw ,(tree-to-svg tree reversed-kw-table
				       reversed-id-table
				       constants-table)))
	     (markup-error-table parser-error-table :parser))))

(defun read-lines (stream)
  (let ((line (read-line stream nil :eof)))
    (unless (eq line :eof)
      (cons line (read-lines stream)))))

(defun read-lines-from-file (file)
  (with-open-file (s file)
    (read-lines s)))

(defun create-numbered-code (lines)
  (let ((line-num 0))
    (mapcar (lambda (line) (list* (incf line-num) #\  line))
	    (mapcar (lambda (line) (coerce line 'list))
		    lines))))

(defun markup-lexer-and-parser (filespec)
  (let ((lexer-output (lexer filespec)))
    (markup* `(:div ,(markup-table nil
				   (create-numbered-code
				    (read-lines-from-file filespec))))
	     `(:div (raw ,(markup-lexer-output lexer-output)))
	     `(:div (raw ,(markup-parser-output lexer-output))))))
