;; near direct implementation of the ISO C++ standard
;; the code is arranged to reflect the "Lexical conventions" chapter
;; developed with n3290.pdf
;; a `grep '2\.' $file` should show the overall structure and state of implementation

(defstruct pp-token
  type ;; :identifier, :whitespace, :literal, etc.
  file ;; what file was it in
  start ;; what was the first character index
  end ;; what was the first index after the token
  string ;; (subseq string start end)
  value ;; an integer or other representation
  )

(defparameter *print-full-pp-token* nil
  "print the full (t) or abbreviated (nil) pp-token")

(defmethod print-object ((obj pp-token) stream)
  (if *print-full-pp-token*
      (call-next-method obj stream)
      (format stream "#S(pp-token :type ~S~@[ :string ~S~]~@[ :value ~S~])"
              (pp-token-type obj)
              (pp-token-string obj)
              (pp-token-value obj))))

(defparameter *filename* nil
  "filename to use when creating tokens")

(defun create-token (type string start end &optional value)
  (make-pp-token
   :type type
   :file *filename*
   :start start
   :end end
   :string (subseq string start end)
   :value value))


#|;; for comparison, see Boost's cpp.re|#

(defconstant slash-t #\Tab "C escape: \t; (code-char 9)")
(defconstant slash-n #\Linefeed "C escape: \n; (code-char 10)")
(defconstant slash-v (code-char 11) "C escape: \v; (code-char 11)")
(defconstant slash-f #\Page "C escape: \f; (code-char 12)")
(defconstant slash-r #\Return "C escape: \r; (code-char 13)")

(defparameter *c++-mode* t "indicate that we are parsing C++ code")

(defun ascii-range (string start end char0 char1)
  (when (< start end)
    (let ((c (char string start)))
      (when (<= (char-code char0) (char-code c) (char-code char1))
        (values (1+ start) c)))))

(defmacro when-match (name rule &body body)
  `(let ((,name ,rule))
     (when ,name
       (values ,name
               ,@body))))

;; these would need to be cpf expansion rules to be truly useful...
(defmacro with-match (context context-symbols rule &body body)
  (destructuring-bind (stringsym startsym aftersym endsym) context-symbols
    (with-slots (string start end) context
      `(let ((,aftersym (cpf-macro ,rule ,context)))
         (when ,aftersym
           (let ((,stringsym ,string)
                 (,startsym ,start)
                 (,endsym ,end))
             (declare (ignorable ,stringsym ,startsym ,endsym))
             ,@body))))))

(defmacro with-match2 (context context-symbols rule &body body)
  (destructuring-bind (stringsym startsym aftersym valsym endsym) context-symbols
    (with-slots (string start end) context
      `(multiple-value-bind (,aftersym ,valsym) (cpf-macro ,rule ,context)
         (when ,aftersym
           (let ((,stringsym ,string)
                 (,startsym ,start)
                 (,endsym ,end))
             (declare (ignorable ,stringsym ,startsym ,endsym))
             ,@body))))))

;; match-filter isn't very useful as it stands
;; it might be usefule as a cpf-list method
(defmacro match-filter (context context-symbols rule &body body)
  (destructuring-bind (stringsym startsym aftersym endsym) context-symbols
    (declare (ignore stringsym startsym endsym))
    `(with-match ,context ,context-symbols ,rule
       (values ,aftersym ,@body))))


;; 2.2, lex.phases
;; skip phases 1 and 2; they are seldom encountered
;; phase 2: warn if the input does not end in a newline
;; this file does phase 3
;; other source files should handle the other phases

;; 2.3, lex.charset
#|
 (defrule basic-source-character-set
    (or
     " " slash-t slash-v slash-f slash-n
     (ascii-range #\a #\z)
     (ascii-range #\A #\Z)
     (ascii-range #\0 #\9)
     and a bunch of punctuation))
|#

(defrule hex-quad
  (and (hexadecimal-digit)
       (hexadecimal-digit)
       (hexadecimal-digit)
       (hexadecimal-digit)))

(defrule universal-character-name
  (or (and "\u" (hex-quad))
      (and "\U" (hex-quad) (hex-quad))))


;; 2.4, lex.trigraph -- low-priority TBD
;; part of "phase 1", rarely used -- implement later

;; 2.5, lex.pptoken -- see below
;; depends on several rules that follow

;; 2.6, lex.digraph -- see below

;; 2.7, lex.token -- see below
;; depends on several rules that follow

;; 2.8, lex.comment
(defun c-comment (string &optional (start 0) (end (length string)))
  (when (starts-with string start end "/*")
    (let ((close (search "*/" string :start2 (+ start 2))))
      (unless close
        (error "unterminated C comment, start: ~A" start))
      (values (+ close 2) (create-token :comment string (+ start 2) close)))))

(defrule c++-comment
  (:cl
   (when *c++-mode*
     (with-match (:context) (string start after end)
       (and "//"
            ;; the standard mentions slash-v and slash-f
            ;; ignore for now since hard to encode
            ;;(repeat 0 nil (exception (any) (or slash-v slash-f slash-n)))
            ;;(repeat 0 nil (or slash-v slash-f (exception whitespace slash-n)))
            (repeat 0 nil (exception (any-char) slash-n))
            (assert slash-n))
       ;; leave the slash-n as a separate token
       (decf after)
       ;; leave out the leading //
       (values after (create-token :comment string (+ start 2) after))))))


;; 2.9, lex.header
(defrule h-char
  (exception (any-char) (or slash-n #\>)))

(defrule h-char-sequence
  (repeat 1 nil (h-char)))


(defrule q-char
  (exception (any-char) (or slash-n #\")))

(defrule q-char-sequence
  (repeat 1 nil (q-char)))

(defparameter *enable-header* nil
  "Section 2.9, header name preprocessing shall only appear within #include...")

(defrule header-name
  (or
   (:cl
    (when *enable-header*
      (match-filter (:context) (string start after end)
          (and #\< (h-char-sequence) #\>)
        (list
         (create-token :punctuation string start (1+ start))
         (create-token :h-include string (1+ start) (1- after))
         (create-token :punctuation string (1- after) after)))))
   (:cl
    (when *enable-header*
      (match-filter (:context) (string start after end)
          (and #\" (q-char-sequence) #\")
        (create-token :q-include string start after))))))


;; 2.10, lex.ppnumber

;; left recursion...
(defrule pp-number
  ;; original, left recursion
  #|
  (or> (digit)
       (and #\. (digit))
       (and (pp-number) (digit))
       (and (pp-number) (identifier-nondigit))
       (and (pp-number) #\e (sign))
       (and (pp-number) #\E (sign))
       (and (pp-number) #\.))
  |#
  ;; equivalent
  (:cl
   (match-filter (:context) (string start after end)
      (and
       (or (and (repeat 0 nil (digit)) #\. (repeat 1 nil (digit)))
           (and (repeat 1 nil (digit)) #\. (repeat 0 nil (digit)))
           (repeat 1 nil (digit)))
       (optional
        (and (or #\e #\E)
             (optional (sign))
             (repeat 0 nil (digit)))))
      (create-token :pp-number string start after))))


#|
 ;; explore solution to left recursion
 ;; simulate (or (and (pp-number) (digit)) (digit))
 (defparameter *pp-start* (cons -1 nil))
 (defrule pp-number
  (:cl
   (progn
     ;;(print (list start *pp-start*))
     (cond
       ;; based on Paull's algorithm
       ;; two cases of sub-recursion
       ;; http://en.wikipedia.org/wiki/Left_recursion#Accommodating_left_recursion_in_top-down_parsing
       ;; given A -> A a1 | ... | A an | b1 | ... | bn
       ;; substitute production

       ;; A -> b1 A' | ... | bm A'
       ;; (i.e. A always fails; then if anything passes, try the rule again)
       ((eql (cdr *pp-start*) :fail)
        nil)

       ;; A' = eps | a1 A' | ... | an A'
       ;; (i.e. A always returns a bogus value)
       ((eql (cdr *pp-start*) :pass)
        (values start nil))

       ((and (= (car *pp-start*) start)
             (cdr *pp-start*))
        (values-list (cdr *pp-start*)))

       ;; detect left-recursion and invoke the two rules
       ((= (car *pp-start*) start) ; left-recursion first detected
        (let (e1 v1 e2 v2)
          ;; find one of the b's
          (let ((*pp-start* (cons start :fail)))
            (setf (values e1 v1)
                  (:parse (or (and (pp-number) (digit)) (digit))))
            (print :v1))
          (unless e1
            (return-from pp-number nil))

          #|
          ;; try to find one of the a's
          (setf start e1) ; using a leaky detail...
          (let ((*pp-start* (cons start nil)))
            (setf (values e2 v2)
                  (:parse (or (and (pp-number) (digit)) (digit)))))
          (setf v2 (list v1 v2))
          |#

          #| alternative, only recurses a couple times...
          |#
          (setf e2 t)
          (do ()
              ((not e2))
            (let ((*pp-start* (cons start (list e1 v1))))
              (setf (values e2 v2)
                    (:parse (or (and (pp-number) (digit)) (digit)))))
            (when e2
              (setf e1 e2
                    start e1
                    ;;v1 (cons v1 v2)
                    )))
            

          ;(print *pp-start*)
          (format t "~A~%" (list :v1 e1 v1 :v2 e2 v2))
          #|
          (if e2
              (values e2 v2)
              (values e1 (cons v1 nil)))))
          |#
          (values e1 v1)))
       (t
        (let ((*pp-start* (cons start nil)))
          (:parse (or (and (pp-number) (digit)) (digit)))))))))
|#
#|
 (defparameter *pp-number* nil)
 (defun pp-number (string &OPTIONAL (START 0) (END (LENGTH STRING)))
  (cond
    ((equal (car *pp-number*) :recur)
     nil)
    ((find start *pp-number*)
     ;; left recursion detected
     (let ((*pp-number* (cons :recur *pp-number*)))
  (if (find start *pp-number*)
      ;; left recursion detected
      (progn
        ;; run, but fail on any further recursions
        )
      ;; normal operation
      (let ((*pp-number* (cons start *pp-number*)))
        
      ))
|#

;; 2.11, lex.name
(defrule nondigit
  (or (ascii-range #\a #\z)
      (ascii-range #\A #\Z)
      #\_))

(defrule digit
  (ascii-range #\0 #\9))

(defrule identifier-nondigit
  (or (nondigit)
      (universal-character-name)
      ;; other implementation-defined characters
      ))

;; left recursion...
(defrule identifier
  ;; left recursive
  #|
  (or> (identifier-nondigit)
       (and (identifier) (identifier-nondigit))
       (and (identifier) (digit)))
  |#
  ;; equivalent
  (:cl
   (match-filter (:context) (string start after end)
       (and (identifier-nondigit) (repeat 0 nil (or (identifier-nondigit) (digit))))
     (create-token :identifier string start after))))
;; todo: identify tokens that are keywords or alternate names
;; but this has to happen *after* the preprocessing (for ## splicing, etc.)

                                                
;; 2.12, lex.key -- see below

;; 2.13, lex.operators

(defrule preprocessing-op-or-punc
  ;; sort in an order so longest matches first
  ;; recognize keywords like and_eq after normal tokenization
  ;; 57 other tokens remain
  (:cl
   (with-match (:context) (string start after end)
     (exception
      (or "{"
          "}"
          "<:"
          ":>"
          "["
          "]"
          "<%"
          "%>"
          "##"
          "#"
          "%:%:"
          "%:"
          #\(
          #\)
          #\;
          #\:
          "..."
          "?"
          "::"
          ";"
          ":"
          ".*"
          "."
          "++"
          "+="
          "+"
          "->*"
          "->"
          "*="
          "*"
          "/="
          "/"
          "--"
          "-="
          "-"
          "<<="
          "<<"
          "<="
          "<"
          ">>="
          ">>"
          ">="
          ">"
          "|="
          "||"
          "|"
          "&="
          "&&"
          "&"
          "^="
          "^"
          "%="
          "%"
          "~"
          "!="
          "!"
          "=="
          "="
          ",")
      ;; avoid accidentally matching the start of a comment
      (or "//" "/*"))
     (values after (create-token :punctuation string start after)))))
   

;; 2.14, lex.literal

;; 2.14.1, lex.literal.kinds -- see below

;; 2.14.2, lex.icon

(defrule nonzero-digit (ascii-range #\1 #\9))

(defrule octal-digit (ascii-range #\0 #\7))

(defrule hexadecimal-digit
  (or
   (ascii-range #\0 #\9)
   (ascii-range #\a #\f)
   (ascii-range #\A #\F)))

;; removed the left-recursions
(defrule decimal-literal
  (:cl
   (match-filter (:context) (string start after end)
       (and (nonzero-digit) (repeat 0 nil (digit)))
     (parse-integer (subseq string start after)))))

(defrule octal-literal
  (:cl
   (match-filter (:context) (string start after end)
       (and #\0 (repeat 0 nil (octal-digit)))
     (parse-integer (subseq string start after) :radix 8))))

(defrule hexadecimal-literal
  (:cl
   (match-filter (:context) (string start after end)
       (or (and "0x" (repeat 1 nil (hexadecimal-digit)))
           (and "0X" (repeat 1 nil (hexadecimal-digit))))
     (parse-integer (subseq string (+ 2 start) after) :radix 16))))

(defrule unsigned-suffix
  (or #\u #\U))

(defrule long-suffix
  (or #\l #\L))

(defrule long-long-suffix
  (or "ll" "LL"))

(defrule integer-suffix
  ;; tweak the order to get the longest match
  (or (and (unsigned-suffix) (optional (long-long-suffix)))
      (and (unsigned-suffix) (optional (long-suffix)))
      (and (long-long-suffix) (optional (unsigned-suffix)))
      (and (long-suffix) (optional (unsigned-suffix)))))

(defrule integer-literal
  ;; again, identify 0x123 as not 0, then "x123"
  (or (and (decimal-literal) (optional (integer-suffix)))
      (and (hexadecimal-literal) (optional (integer-suffix)))
      (and (octal-literal) (optional (integer-suffix)))))


;; 2.14.3, lex.ccon

(defrule simple-escape-sequence
  (and #\\ (or #\' #\" #\? #\\
               #\a #\b #\f #\n #\r #\t #\v)))

(defrule octal-escape-sequence
  (and #\\ (repeat 1 3 (octal-digit))))

(defrule hexadecimal-escape-sequence
  (and #\\ #\x (repeat 1 nil (hexadecimal-digit))))

(defrule escape-sequence
  (or (simple-escape-sequence)
      (octal-escape-sequence)
      (hexadecimal-escape-sequence)))

(defun any-char (string start end)
  (when (< start end)
    (values (1+ start) (char string start))))

(defrule c-char
  (or (exception (any-char) (or #\' #\\ slash-n))
      (escape-sequence)
      (universal-character-name)))

(defrule c-char-sequence
  (repeat 1 nil (c-char)))

(defrule character-literal
  (or (and #\' (c-char-sequence) #\')
      (and "u'" (c-char-sequence) #\')
      (and "U'" (c-char-sequence) #\')
      (and "L'" (c-char-sequence) #\')))

;; 2.14.4, lex.fcon
(defrule sign
  (or #\+ #\-))

(defrule digit-sequence
  (repeat 1 nil (digit)))

(defrule floating-suffix
  (or #\f #\l #\F #\L))

(defrule exponent-part
  (and (or #\e #\E) (optional (sign)) (digit-sequence)))

(defrule fractional-constant
  (or (and (optional (digit-sequence)) #\. (digit-sequence))
      (and (digit-sequence) #\.)))

(defrule floating-literal
  (or (and (fractional-constant) (optional (exponent-part)) (optional (floating-suffix)))
      (and (digit-sequence) (exponent-part) (optional (floating-suffix)))))


;; 2.14.5, lex.string

(defrule encoding-prefix
  (or "u8" #\u #\U #\L))

(defrule s-char
  (or (exception (any-char) (or #\" #\\ slash-n))
      (escape-sequence)
      (universal-character-name)))

(defrule s-char-sequence
  (repeat 1 nil (s-char)))

;; skip raw strings for now
;;(defrule string-literal
;;  (or (and (optional (encoding-prefix)) #\" (optional (s-char-sequence)) #\")
;;      (and (optional (encoding-prefix)) #\R (raw-string))))


(defrule string-literal
  (:cl
   (with-match2 (:context)(string start after val end)
     (and (optional (encoding-prefix))
          (:cl
           (match-filter (:context) (string start after end)
               (and #\" (optional (s-char-sequence)) #\")
              (subseq string (1+ start) (1- after)))))
     (values after (create-token :string string start after val)))))



;; 2.14.6, lex.bool -- TBD
;; 2.14.7, lex.nullptr -- TBD
;; don't these belong as identifying the tokens?

;; 2.14.8, lex.ext -- TBD, low priority

;; 2.14.1 -- impl, relies on TBDs
(defrule literal
  ;; move floating-literal before integer-literal to get correct match
  (or (floating-literal)
      (integer-literal)
      (character-literal)
      (string-literal)
      ;;(boolean-literal)
      ;;(pointer-literal)
      ;;(user-defined-literal)
      ))


;; 2.5, lex.pptoken -- partial impl, has TBDs
(defrule preprocessing-token
  (or (header-name)
      (identifier)
      (pp-number)
      (character-literal)
      ;; skip rarely-used or brand new stuff for now
      ;;(user-defined-character-literal)
      (string-literal)
      ;;(user-defined-string-literal)
      (preprocessing-op-or-punc)
      ;;(any-other-non-whitespace)
       ))


;; 2.6 -- impl TBD, partly in 2.12
;; plan: skip on the proper digraphs for now, only implement the token alternatives listed in 2.12

;; 2.7 -- impl TBD, needs keyword, operator, and punctuator
(defrule token
    (or (identifier)
        (keyword)
        (literal)
        (operator)
        (punctuator)))

;; 2.12 -- impl TBD
;; these apply to full tokens...
(defrule keywords
  (or "alignas"
      "alignof"
      "asm"
      "auto"
      "bool"
      "break"
      "case"
      "catch"
      "char"
      "char16_t"
      "char32_t"
      "class"
      "const"
      "constexpr"
      "const_cast"
      "continue"
      "decltype"
      "default"
      "delete"
      "do"
      "double"
      "dynamic_cast"
      "else"
      "enum"
      "explicit"
      "export"
      "extern"
      "false"
      "float"
      "for"
      "friend"
      "goto"
      "if"
      "inline"
      "int"
      "long"
      "mutable"
      "namespace"
      "new"
      "noexcept"
      "nullptr"
      "operator"
      "private"
      "protected"
      "public"
      "register"
      "reinterpret_cast"
      "return"
      "short"
      "signed"
      "sizeof"
      "static"
      "static_assert"
      "static_cast"
      "struct"
      "switch"
      "template"
      "this"
      "thread_local"
      "throw"
      "true"
      "try"
      "typedef"
      "typeid"
      "typename"
      "union"
      "unsigned"
      "using"
      "virtual"
      "void"
      "volatile"
      "wchar_t"
      "while"))

(defrule alternative-representations
  (or "and"
      "and_eq"
      "bitand"
      "bitor"
      "compl"
      "not"
      "not_eq"
      "or"
      "or_eq"
      "xor"
      "xor_eq"))