(in-package :cl-user) 

(uiop:define-package :signal-translator/syntax-analyzer
    (:nicknames :parser)
  (:use :common-lisp :cl-graph
	:signal-translator/category-array
	:signal-translator/lexical-analyzer)
  (:export #:lexer
           #:eof
           #:digit))

(in-package :parser)

(defun parse (lexer-output)
  (destructuring-bind ((token-table
			(reversed-id-table identifiers-table)
			constants-table)
		       (reversed-kw-table keywords-table)
		       error-table)
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
		 (%predicate :constant (%peek)))

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
		    error-table))
	))
    )
  )

(defun call-parser (filespec)
  (destructuring-bind (tree
		       (((reversed-id-table identifiers-table)
			 constants-table)
			(reversed-kw-table keywords-table)
			error-table))
      (parse (lexer filespec))
    (declare (ignore identifiers-table keywords-table error-table))
    (tree-to-graph tree reversed-kw-table reversed-id-table
		   constants-table)))


(defstruct unique-vertex-impl
  data key)

(defun make-unique-vertex (data)
  (make-unique-vertex-impl
   :data data
   :key (gensym)))

(defun tree-to-graph (tree reversed-kw-table
		      reversed-id-table constants-table)
  (labels
      ((%fill-graph-with-tree (g root branches)
	 (mapc (lambda (branch)
		 (if (atom branch)
		     (add-edge-between-vertexes
			  g root
			  (make-unique-vertex
			   (if (eq branch :empty)
			       branch
			       (test-lexer::dump-token
				(list (unique-vertex-impl-data root)
				      branch)
				reversed-kw-table
				reversed-id-table
				constants-table)))
			  :value "")
		     (let ((branch-root (make-unique-vertex
					 (first branch))))
		       (add-edge-between-vertexes
			g root branch-root :value "")
		       (%fill-graph-with-tree
			g branch-root (rest branch)))))
	       branches)))
    (let ((g (make-instance 'graph-container 
			    :default-edge-type :directed
			    :vertex-test #'equalp)))
      (%fill-graph-with-tree g (make-unique-vertex (first tree))
			     (rest tree))
      (with-open-file (s "./test.dot"
			 :direction :output
			 :if-exists :supersede)
	(graph->dot g s))
      
      (graph->dot-external
       g "./test.svg"
       :type :svg
       :edge-labeler
       (lambda (e s) (princ (cl-graph::value e) s))
       :vertex-labeler 
       (lambda (v s)
	 (let ((data (unique-vertex-impl-data
		      (cl-graph::value v))))
	   (princ
	    (if (symbolp data)
		 (format nil "<~a>" (string-downcase data))
		 data)
	    s)))
       ))))
