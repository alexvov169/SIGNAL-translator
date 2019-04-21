(in-package :cl-user) 

(uiop:define-package :signal-translator/syntax-analyzer
    (:nicknames :parser)
  (:use :common-lisp)
  (:use :signal-translator/category-array
	:signal-translator/lexical-analyzer)
  (:export #:parser
           #:eof
           #:digit))

(in-package :parser)

(defun parse (lexer-output error-handler)
  (destructuring-bind ((token-table
			(reversed-id-table identifiers-table)
			constants-table)
		       (reversed-kw-table keywords-table)
		       lexer-error-table)
      lexer-output
    '(declare (ignore identifiers-table reversed-id-table
		     constants-table
		     reversed-kw-table
		     error-table))
    (let ((token-index 0)
	  (token-end (length token-table))
	  (program-token (gethash "PROGRAM" keywords-table))
	  (semicolon (char-code #\;))
	  (begin-keyword (gethash "BEGIN" keywords-table))
	  (end-keyword (gethash "END" keywords-table))
	  (const-keyword (gethash "CONST" keywords-table)))
      
      (labels ((%peek ()
		 (second (aref token-table token-index)))
	       
	       (%token ()
		 (when (< token-index token-end)
		   (prog1 (%peek)
		     (incf token-index))))

	       (%err-str (expected-p un/expected-what
			  &key (to nil) (before nil) (after nil))
		 (funcall error-handler
			  (create-error-message
			   expected-p un/expected-what
			   :to to :before before :after after)))

	       (%token-eq (token-type token-value &aux (token (%token)))
		 (and (eq (first token) token-type)
		      (eq (second token) token-value)))
	       
	       (%signal-program ()
		 `(:signal-program ,(%program)))
	       
	       (%program ()
		 (if (%token-eq :keyword program-token)
		     `(:program ,(%procedure-identifier)
				,(if (%token-eq :single-char semicolon)
				     (%block)
				     (error "semicolon expected")))
		     (error "program expected")))
	       
	       (%procedure-identifier ()
		 `(:procedure-identifier ,(%identifier)))

	       (%block ()
		 (list :block (%declarations)
		       (if (%token-eq :keyword begin-keyword)
			   (prog1 (%statements-list)
			     (unless (%token-eq :keyword end-keyword)
			       (error "end expected")))
			   (error "begin expected"))))

	       (%declarations ()
		 `(:declarations ,(%constant-declarations)))

	       (%statements-list ()
		 `(:statements-list :empty))
	       
	       (%constant-declarations ()
		 (list :constant-declarations
		       (if (%token-eq :keyword const-keyword)
			   (%constant-declarations-list)
			   :empty)))

	       (%constant-declarations-list ()
		 (cons :constant-declarations-list
		       (if (%predicate :identifier (%peek))
			   (list (%constant-declaration)
				 (%constant-declarations-list))
			   (list :empty))))

	       (%constant-declaration ()
		 (list :constant-declaration
		       (%constant-identifier)
		       (when (%token-eq :single-char
				      (char-code #\=))
			   (prog1 (%constant)
			     (unless (%token-eq :single-char semicolon)
			       (error "semicolon expected"))))))
	       
	       (%constant-identifier ()
		 `(:constant-identifier ,(%identifier)))

	       (%constant ()
		 `(:constant ,(%complex-number)))

	       (%predicate (name-key token)
		 (eq (first token) name-key))

	       (%complex-number-p ()
		 (%predicate :complex-constant (%peek)))

	       (%complex-number ()
		 (if (%complex-number-p)
		     (%token)
		     (error "complex-number expected")))

	       (%identifier-p ()
		 (%predicate :identifier (%peek)))
	       
	       (%identifier ()
		 (if (%identifier-p)
		     (%token)
		     (error "identifier expected"))))
	(list (%signal-program)
	      (list (list (list reversed-id-table identifiers-table)
			  constants-table)
		    (list reversed-kw-table keywords-table)
		    lexer-error-table))
	))
    )
  )


(defun parser (lexer-output)
  (destructuring-bind (error-handler error-table-getter)
      (error-keeper)
    (destructuring-bind (tree
			 (((reversed-id-table identifiers-table)
			   constants-table)
			  (reversed-kw-table keywords-table)
			  lexer-error-table))
	(parse lexer-output error-handler)
      (declare (ignore identifiers-table keywords-table lexer-error-table))
      (list tree reversed-kw-table reversed-id-table constants-table
	    (funcall error-table-getter)))))
