(in-package :cl-user) 

(uiop:define-package :signal-translator/test-parser
    (:nicknames :test-parser)
  (:use :common-lisp :cl-graph)
  (:use :signal-translator/syntax-analyzer
	:signal-translator/lexical-analyzer
	:signal-translator/test-lexer)
  (:export #:call-parser
	   #:tree-to-svg
	   #:vertex-labeller))

(in-package :test-parser)

(defstruct unique-vertex-impl
  data key)

(defun make-unique-vertex (data)
  (make-unique-vertex-impl
   :data data
   :key (gensym)))

(defun vertex-labeller (data)
  (if (symbolp data)
      (format nil "<~a>" (string-downcase data))
      data))

(defun tree-to-svg (tree reversed-kw-table
		    reversed-id-table constants-table)
  (labels
      ((%fill-graph-with-tree (g root branches)
	 (mapc (lambda (branch)
		 (if (atom branch)
		     (add-edge-between-vertexes
		      g root
		      (make-unique-vertex
		       (cond ((or (eq branch :empty)
				  (eq branch :error)
				  (eq branch :ignore))
			      branch)
			     (t (test-lexer::dump-token
				 (list (unique-vertex-impl-data root)
				       branch)
				 reversed-kw-table
				 reversed-id-table
				 constants-table))))
		      :value "")
		     (if (or (eq (unique-vertex-impl-data root)
				 :constant-identifier)
			     (eq (unique-vertex-impl-data root)
				 :procedure-identifier))
			 (let ((branch-root (make-unique-vertex
					     (first (second branch)))))
			   (add-edge-between-vertexes
			    g root branch-root :value "")
			   (%fill-graph-with-tree
			    g branch-root (rest (second branch))))
			 (let ((branch-root (make-unique-vertex
					     (first branch))))
			   (add-edge-between-vertexes
			    g root branch-root :value "")
			   (%fill-graph-with-tree
			    g branch-root (rest branch))))))
	       branches)))
    (let ((g (make-instance 'graph-container 
			    :default-edge-type :directed
			    :vertex-test #'equalp)))
      (%fill-graph-with-tree g (make-unique-vertex (first tree))
			     (rest tree))
      
      (let* ((dot-string
	       (funcall #'graph->dot g nil
			:edge-labeler
			(lambda (e s) (princ (cl-graph::value e) s))
			:vertex-labeler 
			(lambda (v s)
			  (let ((data (unique-vertex-impl-data
				       (cl-graph::value v))))
			    (princ (vertex-labeller data) s)))))
	     (dot-type "-Tsvg")
	     (svg-outstring (make-string-output-stream)))
	#+sbcl
	(sb-ext:run-program cl-graph::*dot-path*
			    (list dot-type)
			    :input (make-string-input-stream dot-string)
			    :output svg-outstring)
	(get-output-stream-string svg-outstring)))))

(defun call-parser (filespec outstream)
  (let ((lexer-output (lexer filespec)))
    (destructuring-bind (tree reversed-kw-table reversed-id-table
			 constants-table parser-error-table)
	(parser lexer-output)
      (tree-to-svg tree reversed-kw-table reversed-id-table
			   constants-table)
      (print-error-table parser-error-table :parser outstream))))
