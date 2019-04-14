;;;;

(asdf:defsystem :signal-translator
  :class :package-inferred-system 
  :description "Lexical analyzer"
  :version "0.0.1"
  :author "Olexiy Vovchok"
  :depends-on (:signal-translator/lexical-analyzer
	       :signal-translator/test-all))
