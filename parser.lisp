;;;; Copyright (C) 2007, Daniel Herring.  All rights reserved.
;;;; See the included COPYING.txt

;; A simple top-down, backtracking parser
;; Modelled after EBNF notation

(defpackage #:ebnf-parser
  (:nicknames #:parser)
  (:use #:common-lisp)
  (:export #:grammar-char
           #:grammar-chartable
           #:grammar-string
           #:grammar-optional
           #:grammar-and
           #:grammar-or
           #:grammar-n
           #:grammar-*
           #:grammar-exception
           #:grammar-func
           #:grammar-rule
           #:start
           #:end
           #:string
           #:*enable-debug*))

(in-package #:ebnf-parser)

(defparameter *enable-debug* nil "compile debug statements into grammar rules")


;;; Internal utilities


(defmacro grammar-call (x)
  "Call function or macro x"
  (cond ((null x) (error "Cannot execute nil."))
        ((stringp x) `(grammar-call ,(read-from-string x)))
        ((symbolp x) `(,x string :start start))
        ((listp x) x)
        (t (error "Cannot call ~S" x))))

(defmacro grammar-wrap (x)
  "Wrap function or macro x as a callback"
  (cond ((null x) (error "Cannot execute nil."))
        ((stringp x) `(grammar-wrap ,(read-from-string x)))
        ((symbolp x) (list 'quote x))
        ((listp x) `(lambda (string &key (start 0)) ,x))
        (t (error "Cannot call ~S" x))))


;;; Parser construction


(defun starts-with (string prefix &key (start 0))
  "Does 'string' begin with 'prefix'?"
  (let ((end (+ start (length prefix)))
        (l (length string)))
    (unless (> end l)
      (string= prefix string :start2 start :end2 end))))

(defmacro grammar-char (c)
  "match = 'c'"
  `(when (and
          (< start (length string))
          (eq ,c (char string start)))
    (values (1+ start) ,(string c))))

(defmacro grammar-chartable (&rest ctable)
  "match = '(first ctable)' | '(second ctable)' | ..."
  `(when (< start (length string))
    (let ((c (char string start)))
      (when (find c ,(format nil "~{~C~}" ctable))
        (values (1+ start) (string c))))))

(defmacro grammar-string (str)
  "match = 'str'"
  (let ((l (length str)))
    (cond ((= l 0) '(values start ""))
          ((= l 1) `(when (and
                           (< start (length string))
                           (eq ,(char str 0) (char string start)))
                     (values (1+ start) ,str)))
          (t 
           `(when (starts-with string ,str :start start)
             (values (+ start ,(length str)) ,str))))))

(defmacro grammar-optional (x)
  "match = [x]"
  `(multiple-value-bind (end value) (grammar-call ,x)
    (if end
        (values end value)
        (values start nil))))

(defmacro grammar-and (first &rest rest)
  "match = first, (grammar-and rest)"
  (if (null rest)
      `(grammar-call ,first)
      `(multiple-value-bind (end value) (grammar-call ,first)
        (when end
          (let ((start end))
          (multiple-value-bind (e v) (grammar-and ,@rest)
            (when e
              (if (listp v)
                  (values e (cons value v))
                  (values e (list value v))))))))))

(defmacro grammar-or (first &rest rest)
  "match = first | (grammar-or rest)"
  (if (null rest)
      `(grammar-call ,first)
      `(multiple-value-bind (end value) (grammar-call ,first)
        (if end
            (values end value)
            (grammar-or ,@rest)))))

(defmacro grammar-n (n x)
  "match = n * x"
  (if (> n 0)
      (let ((n1 (1- n)))
      `(multiple-value-bind (end value) (grammar-call ,x)
        (when end
          (let ((start end))
            (multiple-value-bind (e v) (grammar-n ,n1 ,x)
              (when e
                (if (car v)
                    (values e (cons value v))
                    (values e (list value)))))))))
      'start))

(defun kleene* (f string &key (start 0))
  "match f 0 or more times"
  (multiple-value-bind (end value) (funcall f string :start start)
    (if end
        (multiple-value-bind (e v) (kleene* f string :start end)
          (values e (cons value v)))
        (values start nil))))

(defmacro grammar-* (x)
  "match = {x}"
  `(kleene* (grammar-wrap ,x) string :start start))

(defmacro grammar-exception (x y)
  "match = x - y"
  `(multiple-value-bind (end value) (grammar-call ,x)
    (when end
      (multiple-value-bind (e v) (grammar-call ,y)
        (declare (ignore v))
        (when (not e)
          (values end value))))))


;;; Output control


(defmacro grammar-func (x f)
  "Apply f to the value of x"
  `(multiple-value-bind (end value) (grammar-call ,x)
    (when end
      (values end (,f value)))))


;;; Helper macros

(defmacro grammar-rule-helper (name front body)
  (if front
      `(defun ,name (string &key (start 0))
        ,@front
        ,@body)
      `(defun ,name (string &key (start 0))
        ,@body)))

(defmacro grammar-rule (name &body body)
  "defun wrapper to simplify rule production"
  (if *enable-debug*
      ;; Identify a documentation string
      (let* ((t1 (stringp (car body)))
             (f1 (if t1
                     (car body)
                     nil))
             (b1 (if t1
                     (cdr body)
                     body)))
        ;; Identify a declare clause
        (let* ((t2 (eq (caar b1) 'declare))
               (f2 (cond ((and t2 f1)
                          (cons f1 (list (car b1))))
                         (t2 (list (car b1)))
                         (f1 (list f1))
                         (t nil)))
               (b2 (if t2
                       (cdr b1)
                       b1)))
          `(grammar-rule-helper ,name ,f2 ,(cons `(format t ,(format nil "~A:~A~A" "~5@A" name "~%") start)
                                                 b2))))
      `(grammar-rule-helper ,name nil ,body)))

