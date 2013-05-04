(defclass string-context (context)
  ((string :initarg :string)
   (start :initarg :start)
   (end :initarg :end))
  (:documentation "specialization for parsing a string of text
 While END will often be (length string), making it an explicit parameter should help when the message is known to be shorter.
 For example, many formats encode the message size in a header field."))
#|
possible extensions/optimiztions
- check the length for several rules at once; don't need to check inside
- merge rules into a character-table lookup
- don't generate the value list when it will be discarded (e.g. in token strings)
- flag when start/values can be modified directly
|#

(defmethod print-object ((obj string-context) stream)
  (with-slots (string start end) obj
    (format stream "#.(make-instance 'string-context :string '~A :start '~A :end '~A)~%"
            string start end)))


(defmethod make-context ((type (eql 'string-context)) &key gensym chain)
  (declare (ignore type))
  (make-instance
   'string-context
   :string (cond
             (chain
              (slot-value chain 'string))
             (gensym
              (gensym (symbol-name :string-)))
             (t 'string))
   :start (cond
            ;; how to chain? usually want to start with previous rule's end?
            (gensym
             (gensym (symbol-name :start-)))
            (t 'start))
   :end (cond
          (chain
           (slot-value chain 'end))
          (gensym
           (gensym (symbol-name :end-)))
          (t 'end))))

(defmethod get-position ((context string-context))
  (slot-value context 'start))

(defmethod make-return-list ((context string-context) &key gensym chain)
  (list
   (cond
     (chain
      (slot-value context 'start))
     (gensym
      (gensym (symbol-name :end-)))
     (t 'end))
   (cond
     (gensym
      (gensym (symbol-name :val-)))
     (t 'val))))

(defmethod return-list-binding (retlist (context string-context))
  (cons (list (first retlist) (slot-value context 'start))
        (cdr retlist)))

(defmethod trivial-match ((context string-context) env)
  (declare (ignore env))
  (with-slots (string start end) context
    (declare (ignore string end))
    `(values ,start nil)))

(defmethod trivial-fail ((context string-context) env)
  (declare (ignore context env))
  `(values nil))

(defmethod cpf ((form string) (context string-context) env)
  (declare (ignore env))
  (values
   (with-slots (string start end) context
     `(starts-with ,string ,start ,end ,form))
   t))

(defmethod cpf ((form character) (context string-context) env)
  (declare (ignore env))
  (values
   (with-slots (string start end) context
     `(when (and (< ,start ,end) (char= ,form (char ,string ,start)))
        (values (1+ ,start) ,form)))
   t))

(defmethod cpf-function-lambda ((obj string-context))
  (with-slots (string start end) obj
    `(,string &optional (,start 0) (,end (length ,string)))))

(defmethod insert-function-call (raw-call (context string-context))
  (with-slots (string start end) context
    (destructuring-bind (head &rest rest) raw-call
      `(,head ,string ,start ,end ,@rest))))





;;;;
(defmacro defrule (name &body parse-form &environment env)
  (assert (= (length parse-form) 1))
  (let ((context (make-context 'string-context)))
    `(defun ,name ,(cpf-function-lambda context)
       ,(cpf (car parse-form) context env))))

;; work on a protocol for defrule, allowing compile-time dispatch
(defvar *parse-rules* (make-hash-table)
  "hashtable of the rules that have been defined.  For each rule name, there is a property list recording possible expansions.  The key :source returns the original form.  Context indicators may return pre-compiled forms.")

#| work-in-progress
 (defun get-parse-rule (key context)
  "look up (or create) a parse rule for the given key and context"
  (let* ((props (gethash key *parse-rules*))
         (nonce (gensym))
         (result (getf props context nonce)))
    (assert props)
    (if (eql result nonce)
        (let ((source (getf props :source nonce)))
          (assert (not (eql source nonce)))
          (setf (getf props context) 
                
    
  )))))
|#

;; store an alist (or plist) for each key
;; look up the context (the :form context returns the original source)
;; save pre-compiled forms in a cons; recompile all forms if the source changes
;; then the call site can (funcall (car lookup) args)
