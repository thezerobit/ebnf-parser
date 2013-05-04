(defsystem :extensible-parser
  :name "extensible parsing framework"
  :author "Daniel Herring"
  :license "Boost Software License"
  :description "parsing framework, with an example C++ lexer"
  :serial t
  :components ((:file "extensible-parser")
               (:file "string-context")
               (:file "examples/cpp/lex")
               (:file "examples/cpp/cpp")
               (:file "examples/cpp/phases")
               ))