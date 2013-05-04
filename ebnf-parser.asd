;;;; -*- Lisp -*-

(defpackage #:ebnf-parser-asd
  (:use :cl :asdf))

(in-package :ebnf-parser-asd)

(defsystem ebnf-parser
  :name "EBNF parser"
  :version "20070401"
  :maintainer "Daniel Herring"
  :license "2-clause BSD license (see COPYING.txt for details)"
  :description "ISO-14977 EBNF parser"
  :components ((:file "parser")
               (:file "ISO-14977"
                      :depends-on ("parser"))))
