;; C++ standard, chapter 16: Preprocessing directives
;; (PDF p. 426, printed p. 411)

;; need to allow for whitespace, including comments
(defstruct pp-context
  tokens)

(defmethod print-object ((obj pp-context) stream)
  (write (car (pp-context-tokens obj)) :stream stream))

(defun skip-whitespace (tokens)
  (do ((token (car tokens) (car tokens)))
      ((not (and token
                 (equal :whitespace (pp-token-type token))
                 (not (equal #\Newline (pp-token-value token)))))
       (values tokens nil))
    (setf tokens (cdr tokens))))

(defun match-token-type-and-value (type value tokens)
  (let ((tok (car tokens)))
    (when (and tok
               (eql type (pp-token-type tok))
               (equal value (pp-token-value tok)))
      (values (cdr tokens) tok))))
;; this return convention doesn't work so well -- could never match the last token...
;; maybe return (pp-token-start token remaining-tokens) ?

#|
(defmethod cpf ((form string) (context pp-context) env)
  (if (string= form " ")
      `(skip-whitespace ,(pp-context-tokens context))
      (let ((key (c++-lex-phase3b form)))
        (if (pp-token-value key)
            match-type-and-value
            match-type-and-string
      `(match-token ,(pp-context-tokens context))))
|#

(defrule if-group
  (or (and "#" "if")
      (and "#" "ifdef")
      (and "#" "ifndef")))

(defrule if-section
  (and (if-group)
       (optional (elif-groups))
       (optional (else-group))
       (assert (endif-line))))

(defrule group-part
  (or (if-section)
      (control-line)
      (text-line)
      (and "#" (non-directive))))

(defrule group
  (repeat 1 nil (group-part)))

(defrule preprocessing-file
  (optional (group)))