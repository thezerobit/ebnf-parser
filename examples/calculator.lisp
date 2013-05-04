(defpackage :calculator
  (:nicknames :calc)
  (:documentation "A simple infix calculator")
  (:use :common-lisp :ebnf)
  (:shadow :float
           :integer
           :number)
  (:export :calc
           :run))

(in-package :calculator)

;; Modelled after http://www.boost.org/libs/spirit/doc/grammar.html
(declaim (ftype function factor))

(defgrammar " (* A simple infix calculator *)
 (* Parse numbers *)
 digit='0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9';
 integer=digit, {digit};
 float=integer, '.', integer;
 number=float | integer;

 (* Algebraic operators
    - Multiplicative terms have precedence over additive terms
    - Left-associative
    - Parentheses can override precedence
    *)
 product=factor,{('*'|'/'), factor};
 sum=product, {('+'|'-'), product};
 factor=number | '(', sum, ')';

 (* Process lists of expressions separated by ';'. *)
 calc={sum, ';'};
 "

  ;; Let Lisp convert the numbers for num
  (number (lambda (x) (declare (ignore x))
                  (read-from-string (subseq string start end))))

  (product (lambda (list)
             (let ((x (car list)))
               (dolist (y (cdr list))
                 (case (char (car y) 0)
                   (#\* (setq x (* x (cadr y))))
                   (#\/ (setq x (/ x (cadr y))))))
               x)))
             
  (sum (lambda (list)
         (let ((x (car list)))
           (dolist (y (cdr list))
             (case (char (car y) 0)
               (#\+ (setq x (+ x (cadr y))))
               (#\- (setq x (- x (cadr y))))))
           x)))

  (factor (lambda (x)
            (if (listp x)
                (cadr x)
                x)))

  (calc (lambda (list)
          (mapcar (lambda (x) (car x)) list)))
  )

(defun run ()
  (do ((input (read-line)
              (read-line)))
      ((equal input "") "Done so soon?")
    (multiple-value-bind (e v) (sum input)
      (if v
          (progn
            (format t "= ~A~%" v)
            (when (< e (length input))
              (format t "unmatched text: ~A~%" (subseq input e))))
          (format t "could not process '~A'; enter a blank line to quit~%" input)))))
