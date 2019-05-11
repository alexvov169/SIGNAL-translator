(in-package :cl-user) 

(uiop:define-package :signal-translator/syntax-analyzer
    (:nicknames :parser)
  (:use :common-lisp :alexandria)
  (:use :signal-translator/category-array
        :signal-translator/utils
	:signal-translator/lexical-analyzer)
  (:export #:parser
           #:eof
           #:digit))

(in-package :parser)

(defparameter *in-error* nil)
(defun parse (lexer-output error-handler)
  (let ((*in-error* nil))
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
	'(print token-table)
	(labels ((%peek ()
		   (let ((curr-tok (aref token-table token-index)))
		     (setf current-position (first curr-tok))
		     '(print) (setf current-token (second curr-tok))))
		 
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
		   (setf *in-error* t)
		   '(return-from parse
		     (list (list :error)
		      (list (list (list reversed-id-table identifiers-table)
			     constants-table)
		       (list reversed-kw-table keywords-table)
		       lexer-error-table)))
		   :error)

		 (%token-eq (token-type token-value &aux (token (%token)))
		   (or *in-error*
		       (and (eq (first token) token-type)
			    (eq (second token) token-value))))

		 (%peek-eq (token-type token-value &aux (token (%peek)))
		   (or *in-error*
		       (when (and (eq (first token) token-type)
				  (eq (second token) token-value))
			 (%token)
			 t)))
		 
		 (%signal-program ()
		   (ignore-if-in-error
		     (cons :signal-program (%program))))
		 
		 (%program ()
		   (ignore-if-in-error-else-list
		     (if (%peek-eq :keyword program-token)
			 (list* :program (%procedure-identifier)
				(if (%peek-eq :single-char semicolon)
				    (prog1 (%block)
				      (if (%peek-eq :single-char dot)
					  nil
					  (%err-str t "dot" :after "block")))
				    (%err-str t "semicolon"
					      :after "procedure identifier"
					      :before "block")))
			 (list (%err-str t "PROGRAM"
					 :before "procedure identifier")))))
		 
		 (%procedure-identifier ()
		   (ignore-if-in-error
		     (cons :procedure-identifier (%identifier))))

		 (%block ()
		   (ignore-if-in-error-else-list
		     (list* :block (%declarations)
			    (if (%peek-eq :keyword begin-keyword)
				(prog1 (%statements-list)
				  (unless (%peek-eq :keyword end-keyword)
				    (%err-str t "END" :after "statements list")))
				(list (%err-str t "BEGIN"
						:before "statements list"
						:after "declarations"))))))
		 
		 (%declarations ()
		   (ignore-if-in-error (list :declarations (%constant-declarations))))

		 (%statements-list ()
		   (ignore-if-in-error-else-list
		     (list :statements-list :empty)))
		 
		 (%constant-declarations ()
		   (ignore-if-in-error
		     (cons :constant-declarations
			   (if (%peek-eq :keyword const-keyword)
			       (%constant-declarations-list)
			       (list :empty)))))

		 (%constant-declarations-list ()
		   (ignore-if-in-error-else-list
		     (cons :constant-declarations-list
			   (if (%predicate :identifier (%peek))
			       (cons (%constant-declaration)
				     (%constant-declarations-list))
			       (list :empty)))))

		 (%constant-declaration ()
		   (ignore-if-in-error
		     (list* :constant-declaration (%constant-identifier)
			    (if (%token-eq :single-char equal)
				(prog1 (%constant)
				  (unless (%peek-eq :single-char semicolon)
				    (%err-str t "semicolon"
					     :after "constant")))
				(%err-str t "=" :after "constant identifier"
					       :before "constant")))))
		 
		 (%constant-identifier ()
		   (ignore-if-in-error
		     (cons :constant-identifier (%identifier))))

		 (%constant ()
		   (ignore-if-in-error-else-list
		     (cons :constant (%complex-number))))

		 (%predicate (name-key token)
		   (eq (first token) name-key))

		 (%complex-number-p ()
		   (%predicate :complex-constant (%peek)))

		 (%complex-number ()
		   (ignore-if-in-error-else-list
		     (if (%complex-number-p)
			 (%token)
			 (%err-str t "complex number"))))

		 (%identifier-p ()
		   (%predicate :identifier (%peek)))
		 
		 (%identifier ()
		   (ignore-if-in-error-else-list
		     (if (%identifier-p)
			 (let ((position current-position))
			   (list position (%token)))
			 (%err-str t "identifier")))))
	  
	  (list (%signal-program)
		(list (list (list reversed-id-table identifiers-table)
			    constants-table)
		      (list reversed-kw-table keywords-table)
		      lexer-error-table)))))))


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
