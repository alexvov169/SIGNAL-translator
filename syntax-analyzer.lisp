(in-package :cl-user) 

(uiop:define-package :signal-translator/syntax-analyzer
    (:nicknames :parser)
  (:use :common-lisp :signal-translator/category-array
	:signal-translator/lexical-analyzer)
  (:export #:lexer
           #:eof
           #:digit))

(in-package :parser)

(defun parse (lexer-output)
  (destructuring-bind (token-table keywords-table error-table)
      lexer-output
    
    (let ((token-index 0)
	  (program-token (gethash "PROGRAM" keywords-table))
	  (semicolon-token (char-code #\;))
	  (begin-token (gethash "BEGIN" keywords-table))
	  (end-token (gethash "END" keywords-table))
	  (const-keyword (gethash "CONST" keywords-table)))
      
      (labels ((%peek ()
		 (print (aref token-table token-index)))
	       
	       (%token () 
		 (print (aref token-table token-index))
		 (incf token-index))
	       
	       (%signal-program ()
		 `(:signal-program ,(%program)))
	       
	       (%program ()
		 (if (eq (%token) program-token)
		     `(:program ,(%procedure-identifier)
				,(if (eq (%token) semicolon-token)
				     (%block)
				     (error "semicolon expected")))
		     (error "program expected")))
	       
	       (%procedure-identifier ()
		 `(:procedure-identifier ,(%identifier)))

	       (%block ()
		 `(:blokk ,(%declarations)
			  ,(if (eq (%token) begin-token)
			       (prog1 (%statements-list)
				 (unless (eq (%token) end-keyword)
				   (error "end expected")))
			       (error "begin expected"))))

	       (%declarations ()
		 `(:declarations ,(%constat-declarations)))

	       (%statements-list ()
		 `(:statements-list ,nil))
	       
	       (%constant-declarations ()
		 `(:constant-declarations ,(when (eq (%token) const-keyword)
					     (%constant-declarations-list))))

	       (%constant-declarations-list ()
		 `(:constat-declarations-list ,(%constant-declaration)
					      ,(%constant-declarations-list)))

	       (%constant-declaration ()
		 `(:constant-declaration ,(%constant-identifier)
					 ,(if (eq (%token) (char-code #\=))
					      (prog1 (%constant)
						(unless (eq (%token) semicolon-token)
						  (error "semicolon expected"))))))

	       (%constant-identifier ()
		 `(:constant-identifier ,(%identifier)))

	       (%constant ()
		 `(:constant ,(%complex-number)))

	       (%predicate (name-key token)
		 (eq (first token) name-key))

	       (%complex-number-p ()
		 (%predicate :complex-number (%peek)))

	       (%complex-number ()
		 `(:complex-number ,(if (%complex-number-p)
					(%token)
					(error "complex-number expected"))))

	       (%identifier-p ()
		 t)
	       
	       (%identifier ()
		 (if (%identifier-p)
		     `(:identifier ,(%token))
		     (error "identifier expected")))	       
	       )
	(%signal-program)
	))
    )
  )
