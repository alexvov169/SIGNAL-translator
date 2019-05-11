(in-package :cl-user) 

(uiop:define-package :signal-translator/markup-codegen
    (:nicknames :markup-codegen)
  (:use :common-lisp :cl-markup :split-sequence)
  (:use :signal-translator/lexical-analyzer
	:signal-translator/syntax-analyzer
	:signal-translator/test-lexer
	:signal-translator/test-parser
	:signal-translator/markup-code
        :signal-translator/markup-lexer)
  (:export #:markup-codegen-output))

(in-package :signal-translator/markup-codegen)

(defun markup-codegen-output (codegen-output)
  (destructuring-bind (code error-table) codegen-output
    (markup* `(:div ,(markup-code (split-sequence #\Newline code)))
	     (markup-error-table error-table :|CODE GENERATOR|))))
