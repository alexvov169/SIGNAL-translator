(in-package :cl-user)

(uiop:define-package :signal-translator/code-generator
    (:nicknames :codegen)
  (:use :common-lisp)
  (:use :signal-translator/test-lexer
	:signal-translator/category-array
	:signal-translator/lexical-analyzer)
  (:export #:codegen))

(in-package :codegen)

(defun ignoredp (tree)
  (or (eq tree :ignore) (eq tree :error)))

(defmacro empty-string-if-ignored-else (tree &body body)
  `(if (ignoredp ,tree) "" (progn ,@body)))

(defun codegen-aux (parser-output error-handler)
  (destructuring-bind (tree reversed-kw-table reversed-id-table
		       constants-table parser-error-table)
      parser-output
    (let ((constants-context (make-hash-table :test #'eq))
	  (current-position (list 1 1))
	  (current-token (list :error)))
      (labels ((%unique-id-p (identifier)
		 (not (gethash (second (second identifier))
			       constants-context)))

	       (%err-str (expected-p un/expected-what
			  &key (to nil) (before nil) (after nil))
		 (funcall error-handler
			  (create-error-message
			   current-token current-position
			   expected-p un/expected-what
			   :to to :before before :after after))
		 "")

	       (%add-to-context (identifier)
		 (if (%unique-id-p identifier)
		     (setf (gethash (second (second identifier))
				    constants-context)
			   (gethash (second (second identifier))
				    reversed-id-table))
		     (progn (setf current-token (second identifier))
			    (setf current-position (first identifier))
			    (%err-str t "a unique identifier"
				      :to "make declaration"))))
	       
	       (%output-code (signal-program)
		 (empty-string-if-ignored-else signal-program
		   (empty-string-if-ignored-else (second signal-program)
		     (let ((block (third (second signal-program))))
		       '(print signal-program)
		       '(print block)
		       (if (eq (first signal-program) :error)
			   (%err-str t "tree" :to "generate code")
			   (let ((program-id
				   (%data-id (second
					      (second
					       (second signal-program))))))
			     (format nil ".386
?data SEGMENT
~a?data ENDS

?code SEGMENT
ASSUME cs:?code, ds:?data

~a PROC

~aRET

~a ENDP

FINIT
CALL ~a
?code ENDS

END"
				     (%data-segment-top block)
				     program-id
				     (%code-segment-top block)
				     program-id
				     program-id)))))))

	       (%data-segment-top (block)
		 (empty-string-if-ignored-else block
		   (%data-segment (second (second block)))))

	       (%code-segment-top (block)
		 (empty-string-if-ignored-else block
		   (%code-segment (second (second block)))))
	       
	       (%data-segment (constant-declarations)
		 (empty-string-if-ignored-else constant-declarations
		   (if (eq (second constant-declarations) :empty)
		       ""
		       (%data-decls (second constant-declarations)))))
	       
	       (%data-decls (constant-declarations-list)
		 (empty-string-if-ignored-else constant-declarations-list
		   (if (eq (second constant-declarations-list) :empty)
		       ""
		       (format nil "~a~a"
			       (%data-decl (second constant-declarations-list))
			       (%data-decls (third constant-declarations-list))))))

	       (%data-decl (constant-declaration)
		 (empty-string-if-ignored-else constant-declaration
		   (empty-string-if-ignored-else
		       (second (second constant-declaration))
		     (%add-to-context (second (second constant-declaration)))
		     (format nil "~a DQ 2 DUP (0)~%"
			     (%data-id (second (second constant-declaration)))))))

	       (%data-id (identifier)
		 (if (ignoredp identifier)
		     (format nil "?~a" (gensym "SIG"))
		     (dump-token (second identifier) reversed-kw-table
				 reversed-id-table constants-table)))
	       
	       (%code-segment (constant-declarations)
		 (empty-string-if-ignored-else constant-declarations
		   (if (eq (second constant-declarations) :empty)
		       ""
		       (%mov-inits (second constant-declarations)))))

	       (%mov-inits (constant-declarations-list)
		 (empty-string-if-ignored-else constant-declarations-list
		   (if (eq (second constant-declarations-list) :empty)
		       ""
		       (format nil "~a~a"
			       (%mov-init (second constant-declarations-list))
			       (%mov-inits (third constant-declarations-list))))))
	       
	       (%mov-init (constant-declaration)
		 (empty-string-if-ignored-else constant-declaration
		   (empty-string-if-ignored-else
		       (second (second constant-declaration))
		     (let* ((id-tok (second (second constant-declaration)))
			    (val-tok (second (third constant-declaration)))
			    (val (gethash (second val-tok) constants-table)))
		       '(print val)
		       (if (numberp (second val))
			   (format nil "MOV QWORD PTR ~a[0], ~a
MOV QWORD PTR ~a[8], ~a~%"
				   (%data-id id-tok)
				   (second (first val))
				   (%data-id id-tok)
				   (second val))
			   (format nil "MOV QWORD PTR ~a[0], ~a
MOV QWORD PTR ~a[8], ~a
FLD ~a[8]
FEXP
FISTP ~a[8]
"
				   (%data-id id-tok)
				   (second (first val))
				   (%data-id (second (second constant-declaration)))
				   (second (second val))
				   (%data-id id-tok)
				   (%data-id id-tok)))))))) 
        (%output-code tree)))))

(defun codegen (parser-output)
  (destructuring-bind (error-handler error-table-getter)
      (error-keeper)
    (list (codegen-aux parser-output error-handler)
	  (funcall error-table-getter))))
