(in-package :cl-user) 

(uiop:define-package :signal-translator/test-all
    (:nicknames :lexer-test)
  (:use :common-lisp :signal-translator/test-lexer)
  (:export #:call-lexer))

(in-package :lexer-test)
