;; Example from ISO EBNF spec, section 5.7
(grammar-rule aa
  (grammar-string "A"))

(grammar-rule bb
  (grammar-and
   (grammar-n 3 aa)
   (grammar-string "B")))

(grammar-rule cc
  (grammar-and
   (grammar-n 3 (grammar-optional aa))
   (grammar-string "C")))

(grammar-rule dd
  (grammar-and
   (grammar-* aa)
   (grammar-string "D")))

(grammar-rule ee
  (grammar-and
   aa
   (grammar-* aa)
   (grammar-string "E")))

(grammar-rule ff
  (grammar-and
   (grammar-n 3 aa)
   (grammar-n 3 (grammar-optional aa))
   (grammar-string "F")))

(grammar-rule gg
  (grammar-and
   (grammar-n 3 (grammar-* aa))
   (grammar-string "D")))
