;; Simple grammar
(defun parse-token (string &key (start 0))
  "token := 'a' | 'b'"
  (grammar-or
   (grammar-string "a")
   (grammar-string "b")))

(defun parse-list (string &key (start 0))
  "list := '(', {token}, ')'"
  (grammar-and
   (grammar-string "(")
   (grammar-* parse-token)
   (grammar-string ")")))
