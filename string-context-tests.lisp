(defrule test-cl
  (:cl (and (:parse "hi"))))

(defrule test-and0 (and "hi" (and)))

(defrule test-and (and "hello" " " "world"))
;;(test-and "hello world")

(defrule test-or0 (or (or) "hi"))

(defrule test-or
  (or "hello" "world"))
;; (test-or "hello")
;; (test-or "world")

(defrule test-composition (or (test-and) "no"))

(defrule test-composition2
  (and (or "a" "b")
       (or "c" "d")))

(defrule test-or>
  (or> "a" "abc" "ab" "abcd"))

(defrule test-repeat
  (repeat 1 nil #\a))

(defrule test-repeat2
  (and (repeat 0 1 #\a)
       (repeat 1 nil #\b)))


