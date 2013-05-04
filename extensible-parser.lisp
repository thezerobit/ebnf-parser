#| extensive redesign of the ebnf parser
- allow matching in strings, lists, and other source types (extensible)
- allow writing expressions using normal CL grammar but evaluating in a parse-specific way
- allow easily embedding custom parse forms (including plain CL)
- quote/quasiquote-style mechanism for nesting plain lisp functions
  (:cl (f (:parse rule)))
|#

;; (start end) = bounding index designators, e.g. (int int) or (int nil)
;; except returning end==nil indicates no match; so this might not be a good convention for parameters

;; parser compiler similar to "On Lisp" section 19.5
;; use generic functions to register actions

#| Parser calling convention

Given a form,

if an atom,
- try to expand symbol macros, take the values of constants, and re-dispatch
- if T, return match without advancing
- if NIL, return fail without advancing
- else try to match the literal (using (format nil "~S" f) if needed)

if a list,
- try dispatching to cpf-list
- if (car list) is :cl, then (:cl x) -> x
  and recursively change (:parse x) to (setf context (cpf x context))
  (
|#

#|
parser protocol
args: (sequence start end)
return: nil or (next-start value)

transform protocol similar to Boost::Spirit
see http://www.boost.org/doc/libs/1_48_0/libs/spirit/doc/html/spirit/qi/reference/action.html
args: (sequence start next-start end value)
return: nil or (next-start value)
note that the fancy return-value isn't necessary; just wrap the parser...

old protocol had start
|#

#|
definition: A ``parse-form'' is a shorthand notation for specifying parsers; it leaves out the (seq start end) parameters used to pass stream context.

the parse-form (x y z) is expanded to (x seq start end y z)
a parse-form atom is taken to be a literal token
|#

;; parser compiler similar to "On Lisp" section 19.5
;; use generic functions to register actions

;;;; PROTOCOL

(defgeneric cpf (form context env)
  (:documentation "Main entry point for expanding parse forms.
 CPF = compile parse form.
 Given a parse expression and a parsing context, return (values expansion expanded-p)."))

(defgeneric cpf-list (car form context env)
  (:documentation "CPF dispatches list forms to this function.
 The user should register new parse forms by creating the proper method specializations."))

(defmacro cpf-macro (form context &environment env)
  "delay macroexpansion"
  (cpf form context env))

(defclass context nil nil
  (:documentation "specialize this class for different types of input"))

(defclass list-context (context)
  (top here end)
  (:documentation "specialization for parsing a list of tokens"))

(defclass array-context (context)
  ((array :initarg :array :reader array-context-array)
   (start :initarg :start :reader array-context-start)
   (end :initarg :end :reader array-context-end))
  (:documentation "specialization for parsing an array of tokens"))

(defgeneric make-context (type &key gensym chain))

(defun chain-context (context)
  (make-context (type-of context) :gensym t :chain context))

(defgeneric get-position (context))


(defgeneric make-return-list (context &key gensym chain)
  (:documentation "list of symbols representing the function's return values"))
;; if chain is true, the re-use the context's start as the return's end

(defun chain-return-list (context)
  (make-return-list context :gensym t :chain t))

(defgeneric return-list-binding (retlist context))

(defgeneric trivial-match (context env)
  (:documentation "return a match without consuming input"))

(defgeneric trivial-fail (context env)
  (:documentation "return a fail, regardless of input"))

;; print a warning when a form is not recognized
;; in (parse form), form may be incorrect; mark as (parse (cl form)) if intentional
(define-condition warn-parse-form-not-recognized (style-warning)
  ((form :initarg :form :reader warn-pfnr-form))
  (:report
   (lambda (c s)
     (format s "parse form was not recognized: ~A"
             (warn-pfnr-form c)))))

#|
 (defmethod cpf-list (car form seq start end)
  ;; default method: don't modify the form...
  form)
|#

(defun starts-with (string start end prefix)
  "Does 'string' begin with 'prefix'?"
  (let ((stop (+ start (length prefix))))
    (when (and (<= stop (or end (length string)))
               (string= prefix string :start2 start :end2 stop))
      (values stop prefix))))

(defmethod cpf ((form string) (context array-context) env)
  (declare (ignore env))
  (values
   (with-slots (array start) context
     `(when (string= (aref ,array ,start) ,form)
        (values ,form (`+ ,start))))
   t))

(defmethod cpf ((form list) context env)
  (cpf-list (car form) form context env))

(defmethod cpf ((form symbol) context env)
  "expand symbol macros and constants"
  ;; try to expand a symbol-macro
  (multiple-value-bind (exp exp-p) (macroexpand form env)
    (if exp-p
        (cpf exp context env)
        ;; not a symbol-macro, try to expand a constant
        (let* ((p (constantp form))
               (val (and p (symbol-value form))))
          ;; don't recurse on keywords and other self-evaluating forms!
          (if (and p (not (eql val form)))
            (cpf (symbol-value form) context env)
            ;; what to do here should depend on the context...
            (if (eql form t)
                ;; special-case: t always matches without consuming input
                (trivial-match context)
                (error "do not know how to expand symbol ~A" form)))))))

(defmethod cpf ((form null) context env)
  "always fail without consuming input"
  (declare (ignore form context env))
  nil)

#| invocation options
- generic functions
 (defgeneric name (context &optional args))
 (defmethod name ((context string) &optional start end))

- separate package for each context
 (defun string-parser:name (string start end))

- store implementations in *parse-rules* table
 (name) -> `(funcall ,(lookup-rule name) string start end)
or to allow changes to the rule,
 (name) -> `(funcall (car ,(lookup-rule name) string start end))
|#

(defgeneric cpf-function-lambda (context)
  (:documentation "return the nominal lambda list for a given context"))

(defgeneric insert-function-call (raw-call context)
  (:documentation "insert parameters from the context to complete the call"))


;; the following is why some cpf expansions return (values exp t)
;; should decide how to clean everything up (add or remove for consistency)
#|
 (defun cpf-cl-key (form context &optional (key 'parse))
  "recurse through a normal CL form, processing sublists that begin with KEY as parse forms"
  (if (listp form)
      (let ((modified nil))
        (values
         (loop for x in form
            collecting
              (if (and (listp x) (eql (car x) key))
                  (multiple-value-bind (exp exp-p) (cpf (cdr x) context)
                    (if exp-p
                        (setf modified t)
                        (warn 'warn-pfnr-form :form exp))
                    exp)
                  x))
         modified))
      form))
|#

(defmethod cpf-list (car form context env)
  "expand unknown forms as parse calls"
  (declare (ignore car env))
  ;; issue a warning for some forms?
  ;; e.g. (:keyword ...)
  (insert-function-call form context))

(defmethod cpf-list ((car (eql :parse)) form context env)
  (declare (ignore car context env))
  (error ":parse not in :cl - ~S" form))

(defmethod cpf-list ((car (eql :context)) form context env)
  (declare (ignore car context env))
  (error ":context not in :cl - ~S" form))


(defun expand-nested-parse (form context &optional (key :parse))
  ;; don't pass env; it will be picked up by the call site...
  "Destructively recurse through the form, expanding (:parse x) into a (cpf x) call site.  Also expands (:context) to the parse context object (e.g. for macros)."
  (when (listp form)
    ;; in (x1 x2 x3), replace xi if it matches (key y)
    (mapl (lambda (x)
            (let ((c (car x)))
              (when (listp c)
                (cond
                  ((eql (car c) key)
                   (unless (= (length c) 2)
                     (error "expected (:parse rule), got ~S" c))
                   (setf (car x) `(cpf-macro ,(cadr c) ,context)))
                  ((equal c (list :context))
                   (setf (car x) context))
                  (t
                   (expand-nested-parse c context key))))))
          form))
  form)

(defmethod cpf-list ((car (eql :cl)) form context env)
  "bind a cpf context and invoke any nested call sites"
  (declare (ignore car env))
  ;; should this bind a new context?  skip for now...
  (unless (= (length form) 2)
    (error "expected (:cl form), got ~S" form))
  ;; need to wrap and unwrap the argument so (:cl (:parse ...)) works
  (car (expand-nested-parse (cdr form) context)))

#| original nested method
 (defmethod cpf-list ((car (eql and)) form context)
  "Apply the parse forms in order, returning a list of their results or nil if any do not match."
  ;;"return nil if any term failed or (values last-end (list val1 ... valn)) if all passed"
  (let ((term (cadr form))
        (rest (cddr form)))
    (with-slots (string start end) context
      (values
       (let ((e1 (gensym (symbol-name :e1-)))
             (v1 (gensym (symbol-name :v1-))))
         (if rest
             (let ((new-context
                    (make-instance 'string-context
                                   :string string
                                   :start e1
                                   :end end))
                   (erest (gensym (symbol-name :erest-)))
                   (vrest (gensym (symbol-name :vrest-))))
               `(multiple-value-bind (,e1 ,v1) ,(cpf term context)
                  (when ,e1
                    (multiple-value-bind (,erest ,vrest) ,(cpf (cons and rest) new-context)
                      (when ,erest (values ,erest (cons ,v1 ,vrest)))))))
             `(multiple-value-bind (,e1 ,v1) ,(cpf term context)
                (when ,e1
                  (values ,e1 (cons ,v1 nil))))))
       t))))
|#

(defmacro pushback (obj tail)
  "push obj to the tail of the list, and update the tail"
  `(setf (cdr ,tail) (cons ,obj nil)
         ,tail (cdr ,tail)))

;; new flat method (less nesting is easier to read?)
(defmethod cpf-list ((car (eql 'and)) form context env)
  (declare (ignore car))
  (unless (cdr form) ; style tweak, doesn't change runtime, mimics (cl:and)
    (return-from cpf-list (trivial-match context env)))
  (let* ((args (cdr form))
         (new-context (chain-context context))
         (blocksym (gensym (symbol-name :and-)))
         (retlist (chain-return-list new-context))
         (listsym (gensym (symbol-name :list-)))
         (tailsym (gensym (symbol-name :tail-)))
         (inner
           `(let* (,@(return-list-binding retlist context)
                   (,listsym (cons nil nil))
                   (,tailsym ,listsym))))
         (last (last inner 1)))
    (dolist (f args)
      (pushback
       `(setf (values ,@retlist) ,(cpf f new-context env))
       last)
      (pushback
       `(unless ,(car retlist)
          (return-from ,blocksym))
       last)
      ;; note: assumes a single secondary return value...
      (pushback
       `(pushback ,(second retlist) ,tailsym)
       last))
    (pushback `(values ,(car retlist) (cdr ,listsym))
              last)
    `(block ,blocksym ,inner)))


#| original recursive implementation
 (defmethod cpf-list ((car (eql 'or)) form context env)
  "Apply the parse forms in order, returning the value of the first that matches or nil in none match."
  (with-slots (string start end) context
    (let ((term (cadr form))
          (rest (cddr form))
          (e1 (gensym (symbol-name :e1-)))
          (v1 (gensym (symbol-name :v1-))))
      (values
       (if rest
           `(multiple-value-bind (,e1 ,v1) ,(cpf term context env)
              (if ,e1
                  (values ,e1 ,v1)
                  ,(cpf (cons 'or rest) context env)))
          (cpf term context env))
       t))))
|#

;; new flat implementation
(defmethod cpf-list ((car (eql 'or)) form context env)
  (declare (ignore car))
  (unless (cdr form) ; style tweak, doesn't change runtime, mimics (cl:or)
    (return-from cpf-list nil))
  (let* ((args (cdr form))
         (blocksym (gensym (symbol-name :or-)))
         (retlist (make-return-list context :gensym t))
         (vlist (cons 'cl:values retlist))
         (inner
           `(let* ,retlist))
         (last (last inner 1)))
    (dolist (f args)
      (pushback
       `(setf ,vlist ,(cpf f context env))
       last)
      (pushback
       `(when ,(car retlist)
          (return-from ,blocksym ,vlist))
       last))
    `(block ,blocksym ,inner)))

(defmethod cpf-list ((car (eql 'or>)) form context env)
  "return the rule that has the longest match"
  (declare (ignore car))
  (unless (cdr form) ; style tweak, doesn't change runtime, mimics (cl:or)
    (return-from cpf-list nil))
  (let* ((args (cdr form))
         (blocksym (gensym (symbol-name :or-)))
         (retlist (make-return-list context :gensym t))
         (bestretlist (make-return-list context :gensym t))
         (inner
           `(let* (,@(return-list-binding retlist context)
                   ,@(return-list-binding bestretlist context))))
         (last (last inner 1)))
    (dolist (f args)
      (pushback
       `(setf (values ,@retlist) ,(cpf f context env))
       last)
      (pushback
       ;; assumes (car retlist) is a value indicating progress...
       `(when (and ,(car retlist) (> ,(car retlist) ,(car bestretlist)))
          (setf (values ,@bestretlist) (values ,@retlist)))
       last))
    (pushback
     `(when ,(car bestretlist)
        (values ,@bestretlist))
     last)
    `(block ,blocksym ,inner)))


(defmethod cpf-list ((car (eql 'optional)) form context env)
  (declare (ignore car))
  (cpf (cons 'repeat (cons 0 (cons 1 (cdr form)))) context env))

(defmethod cpf-list ((car (eql 'repeat)) form context env)
  "(repeat min max form) where min is 0 or more and max is an integer or nil for unlimited"
  (declare (ignore car))
  (destructuring-bind (min max body) (cdr form)
    (let* ((c (gensym (symbol-name :count-)))
           (new-context (chain-context context))
           (retlist (chain-return-list new-context))
           (le (gensym (symbol-name :last-end-)))
           (v (gensym (symbol-name :val-))))
      (values
       `(do ((,c 0)
             ,@(return-list-binding retlist context)
             (,le ,(if (= 0 min)
                       (get-position context)
                       nil))
             (,v nil))
            (,(if max
                  `(or (not ,(car retlist))
                       (>= ,c ,max))
                  `(not ,(car retlist)))
             (when (>= ,c ,min)
               (values ,le (reverse ,v))))
          (setf (values ,@retlist) ,(cpf body new-context env))
          (when ,(car retlist)
            (incf ,c)
            (setf ,le ,(car retlist))
            (push ,(second retlist) ,v)))
       t))))

(defmethod cpf-list ((car (eql 'exception)) form context env)
  "(exception A B) -> match if A matches but B does not"
  (declare (ignore car))
  (assert (= (length form) 3))
  (destructuring-bind (pass fail) (cdr form)
    (let* ((retlist (make-return-list context :gensym t)))
      `(multiple-value-bind ,retlist ,(cpf pass context env)
         (when ,(car retlist)
           (unless ,(cpf fail context env)
             (values ,@retlist)))))))

(defmethod cpf-list ((car (eql 'assert)) form context env)
  "(assert A) -> A or an exception if no match"
  (declare (ignore car))
  (unless (= (length form) 2)
    (error "expected (assert X), got ~A" form))
  (let* ((retlist (make-return-list context :gensym t)))
    `(multiple-value-bind ,retlist ,(cpf (cadr form) context env)
       (unless ,(car retlist)
         (error ,(format nil "rule did not match: ~A~%at index ~~A" form) ,(get-position context)))
       (values ,@retlist))))


#|
(defmacro defrule (name &body body)
  `(progn
     (defun ,name (seq start end)
       (cpf ,body seq start end))
     (demethod cpf-list ((car (eql ',name)) seq start end)
             (cpf ,body seq start end)))
|#

;;;; ARRAY IMPLEMENTATION

#| stale
 (defmethod cfp ((form string) (context array-context))
  "match a string in the array"
  (with-slots (array start) context
    (values
     `(when (string= ,form (aref ,array ,start))
        (values ,form (1+ ,start)))
     t)))
|#
