(use-package "EBNF-PARSER")


;; Simple tests
(grammar-rule parse-test
  "match := 'a' | 'b'"
  (grammar-or
   (grammar-string "a")
   (grammar-string "b")))

;; This is ambiguous; for now, 'a'* always matches...
(defun parse-test (string &key (start 0))
  "match := 'a'* | 'b'"
  (grammar-or
   (grammar-* (grammar-string "a"))
   (grammar-string "b")))

(defun parse-test (string &key (start 0))
  "match := 'a', 'b'"
  (grammar-and
   (grammar-string "a")
   (grammar-string "b")))

(defun parse-test (string &key (start 0))
  "match = {('a' | 'b' | 'c') - 'b'}"
  (grammar-*
   (grammar-exception
    (grammar-or (grammar-string "a")
                (grammar-string "b")
                (grammar-string "c"))
    (grammar-string "b"))))

(grammar-rule parse-test
  (grammar-func
   (grammar-* (grammar-string "a"))
   (lambda (x) (format nil "~{~A~}" x))))

(grammar-rule parse-test
  "match = {'a'}, 'b'"
  (grammar-func
   (grammar-and (grammar-* (grammar-string "a"))
                (grammar-string "b"))
   (lambda (list)
     (destructuring-bind (x y) list
       (format nil "~{~A~}, ~A" x y)))))

(grammar-rule parse-test
  (grammar-string "abc"))

(grammar-rule parse-test
  (grammar-string ""))
