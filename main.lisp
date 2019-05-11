(in-package :cl-user) 

(uiop:define-package :signal-translator/main
    (:nicknames :main)
  (:use :common-lisp :cl-markup)
  (:use :signal-translator/lexical-analyzer
	:signal-translator/syntax-analyzer
	:signal-translator/code-generator
	:signal-translator/markup-code
	:signal-translator/markup-lexer
	:signal-translator/markup-parser
	:signal-translator/markup-codegen)
  (:export #:markup-all
	   #:main))

(in-package :main)

(defun read-lines (stream)
  (let ((line (read-line stream nil :eof)))
    (unless (eq line :eof)
      (cons line (read-lines stream)))))

(defun read-lines-from-file (file)
  (with-open-file (s file)
    (read-lines s)))

(defun markup-all (filespec)
  (let* ((lexer-output (lexer filespec))
	 (parser-output (parser lexer-output))
	 (codegen-output (codegen parser-output)))
    (markup* `(:div ,(markup-code (read-lines-from-file filespec)))
	     `(:div (raw ,(markup-lexer-output lexer-output)))
	     `(:div (raw ,(markup-parser-output parser-output)))
	     `(:div (raw ,(markup-codegen-output codegen-output))))))

(defun markup-lexer-and-parser (filespec)
  (let* ((lexer-output (lexer filespec))
	 (parser-output (parser lexer-output)))
    (markup* `(:div ,(markup-code (read-lines-from-file filespec)))
	     `(:div (raw ,(markup-lexer-output lexer-output)))
	     `(:div (raw ,(markup-parser-output parser-output))))))

(defun main (argv)
  (if (rest argv)
      (with-open-file (s (third argv)
			 :direction :output
			 :if-exists :supersede
			 :if-does-not-exist :create)
	(format s (markup-all (second argv))))
      (error "Please, specify input file")))

(defun main-parser (argv)
  (if (rest argv)
      (with-open-file (s (third argv)
			 :direction :output
			 :if-exists :supersede
			 :if-does-not-exist :create)
	(format s (markup-lexer-and-parser (second argv))))
      (error "Please, specify input file")))
