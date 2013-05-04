(defpackage :zipcode
  (:documentation "Parse a USPS zipcode.")
  (:use :common-lisp :ebnf)
  (:export :zipcode))

(in-package :zipcode)

(defgrammar "
 digit='0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9';
 zipcode= 5*digit, '-', 4*digit-5*digit
        | 5*digit - (5*digit, ('-' | digit));"

  (zipcode (lambda (x) (declare (ignore x))
                   (subseq ebnf:string start end))))