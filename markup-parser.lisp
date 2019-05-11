(in-package :cl-user) 

(uiop:define-package :signal-translator/markup-parser
    (:nicknames :markup-parser)
  (:use :common-lisp :cl-markup)
  (:use :signal-translator/test-lexer
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
					    (cond ((or (eq :empty branch)
						       (eq :error branch)
						       (eq :ignore branch))
						   (vertex-labeller branch))
						  (t (vertex-labeller
						      (%dump-token root branch))))))
				(if (or (eq root :procedure-identifier)
					(eq root :constant-identifier))
				    (%aux (first (second branch))
					  (rest (second branch)))
				    (%aux (first branch) (rest branch)))))
			  branches)))))
    '(print tree)
    (list :ul (%aux (first tree) (rest tree)))))

(defun markup-parser-output (parser-output)
  (destructuring-bind (tree reversed-kw-table reversed-id-table
		       constants-table parser-error-table)
      parser-output
    (markup* `(:div ,(markup-tree tree reversed-kw-table
				  reversed-id-table constants-table))
	     `(:div (raw ,(tree-to-svg tree reversed-kw-table
				       reversed-id-table
				       constants-table)))
	     (markup-error-table parser-error-table :parser))))
