(in-package :cl-user) 

(uiop:define-package :signal-translator/test-all
    (:nicknames :test-all)
  (:use :common-lisp :signal-translator/test-lexer
	:signal-translator/test-parser
        :signal-translator/markup-lexer
	:signal-translator/markup-parser)
  (:export #:call-lexer))

(in-package :test-all)
