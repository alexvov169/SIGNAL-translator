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

'(defconstant +first-following+
  (list :signal-program (list nil
			      (list nil))
	:program (list (list "PROGRAM")
		       (list nil))
	:block (list nil
		     (list "BEGIN")
		     )
	:declarations (list (list ))))

(defparameter in-error nil)
(defun parse (lexer-output error-handler)
  (destructuring-bind ((token-table
			(reversed-id-table identifiers-table)
			constants-table)
		       (reversed-kw-table keywords-table)
		       lexer-error-table)
      lexer-output
    (let ((token-index 0)
	  (token-end (length token-table))
	  (program-token (gethash "PROGRAM" keywords-table))
	  (semicolon (char-code #\;))
	  (dot (char-code #\.))
	  (equal (char-code #\=))
	  (begin-keyword (gethash "BEGIN" keywords-table))
	  (end-keyword (gethash "END" keywords-table))
	  (const-keyword (gethash "CONST" keywords-table))
	  (current-token nil)
	  (current-position nil))
      (print token-table)
      (labels ((%peek ()
		 (let ((curr-tok (aref token-table token-index)))
		   (setf current-position (first curr-tok))
		   (print (setf current-token (second curr-tok)))))
	       
	       (%token ()
		 (when (< token-index token-end)
		   (prog1 (%peek)
		     (incf token-index))))
	       
	       (%err-str (expected-p un/expected-what
			  &key (to nil) (before nil) (after nil))
		 (funcall error-handler
			  (create-error-message
			   current-token current-position
			   expected-p un/expected-what
			   :to to :before before :after after))
		 (return-from parse)
		 :error)

	       (%token-eq (token-type token-value &aux (token (%token)))
		 (and (eq (first token) token-type)
		      (eq (second token) token-value)))

	       (%peek-eq (token-type token-value &aux (token (%peek)))
		 (when (and (eq (first token) token-type)
			    (eq (second token) token-value))
		   (%token)
		   t))
	       
	       (%signal-program ()
		 (list :signal-program (%program)))
	       
	       (%program ()
		 (if (%peek-eq :keyword program-token)
		     (list :program (%procedure-identifier)
			   (if (%peek-eq :single-char semicolon)
			       (prog1 (%block)
				 (if (%peek-eq :single-char dot)
				     (%err-str t "dot" :after "block")))
			       (%err-str t "semicolon"
					 :after "procedure identifier"
					 :before "block")))
		     (%err-str t "PROGRAM"
			       :before "statements list"
			       :after "declarations")))
	       
	       (%procedure-identifier ()
		 (list :procedure-identifier (%identifier)))

	       (%block ()
		 (list :block (%declarations)
		       (if (%peek-eq :keyword begin-keyword)
			   (prog1 (%statements-list)
			     (unless (%peek-eq :keyword end-keyword)
			       (%err-str t "END" :after "statements list")))
			   (%err-str t "BEGIN"
				     :before "statements list"
				     :after "declarations"))))

	       (%declarations ()
		 (list :declarations (%constant-declarations)))

	       (%statements-list ()
		 (list :statements-list :empty))
	       
	       (%constant-declarations ()
		 (list :constant-declarations
		       (if (%peek-eq :keyword const-keyword)
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
		       (when (%token-eq :single-char equal)
			   (prog1 (%constant)
			     (unless (%peek-eq :single-char semicolon)
			       (%err-str t "semicolon"
					 :after "constant"))))))
	       
	       (%constant-identifier ()
		 (list :constant-identifier (%identifier)))

	       (%constant ()
		 (list :constant (%complex-number)))

	       (%predicate (name-key token)
		 (eq (first token) name-key))

	       (%complex-number-p ()
		 (%predicate :complex-constant (%peek)))

	       (%complex-number ()
		 (if (%complex-number-p)
		     (%token)
		     (%err-str t "complex number")))

	       (%identifier-p ()
		 (%predicate :identifier (%peek)))
	       
	       (%identifier ()
		 (if (%identifier-p)
		     (%token)
		     (%err-str t "identifier"))))
	
	(list (%signal-program)
	      (list (list (list reversed-id-table identifiers-table)
			  constants-table)
		    (list reversed-kw-table keywords-table)
		    lexer-error-table))))))


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
