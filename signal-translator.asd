;;;;

(asdf:defsystem :signal-translator
  :class :package-inferred-system 
  :description "Signal translator"
  :version "0.0.1"
  :author "Oleksii Vovchok"
  :depends-on (#| :signal-translator/syntax-analyzer |#
	       :signal-translator/lexical-analyzer
	       :signal-translator/test-all))
