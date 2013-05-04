;; 2.2 -- impl

;; phase 1
;; take input string and produce an output string
;; trigraphs to single characters
;; \r\n to \n
;; complain if anything doesn't match the basic-source-character-set
;; this should get line numbers correct and columns close enough
;; if desired, some structure could track offsets on affected lines

(defun c++-lex-phase2 (source-string)
  "strip out backslash-newline sequences -- need to preserve line numbers..."
  (let* ((match (format nil "\\~%" slash-n))
         (count
          (loop
             for c = 0 then (1+ c)
             for p0 = 0 then (+ 2 pos)
             for pos = (search match source-string :start2 p0)
             while pos
             finally (return c)))
         (result (make-string (- (length source-string) (* 2 count)))))
    (loop
       for r0 = 0 then (+ r0 (- pos p0))
       for p0 = 0 then (+ 2 pos)
       for pos = (search (format nil "\\~%" slash-n) source-string :start2 p0)
       do (replace result source-string :start1 r0 :start2 p0 :end2 (or pos (length source-string)))
       while pos)
    result))

;; approximate phase 3
(defrule whitespace
  (or (c-comment) (c++-comment)
      (:cl (with-match2 (:context) (string start after val end)
             (or slash-t slash-n slash-v slash-f slash-r #\Space)
             (values after
                     (make-pp-token :type :whitespace
                                    :file *filename*
                                    :start start
                                    :end after
                                    :value val))))))


(defrule c++-lex-phase3
  ;; need to detect "#" "include" and bind *enable-header* to t
  ;; need to make sure whitespace exists between certain tokens?
  (repeat 0 nil (or (preprocessing-token) (whitespace))))

(defrule c++-lex-phase3-helper
    (or (preprocessing-token) (whitespace)))

(defun c++-lex-phase3b (string &optional (start 0) (end (length string)))
  (do ((after start)
       val
       (farthest start)
       vlist
       ;; simple state machine to detect #include statements
       (include-state 0))
      ((not after)
       (when (> farthest start)
         (values farthest (reverse vlist))))
    (let ((*enable-header* (= include-state 2)))
      (setf (values after val) (c++-lex-phase3-helper string after end)))
    (when after
      (setf farthest after)
      (if (listp val)
          (setf val
                (dolist (v val last-val)
                  (push v vlist)
                  (setf last-val v)))
          (push val vlist))
      (ecase include-state
        (0 (when (and (equal :punctuation (pp-token-type val))
                      (string= "#" (pp-token-string val)))
             (incf include-state)))
        (1 (cond
             ((equal :whitespace (pp-token-type val))
              ;; no change
              )
             ((and (equal :identifier (pp-token-type val))
                   (string= "include" (pp-token-string val)))
               (incf include-state))
             (t (setf include-state 0))))
        (2 (cond
             ((equal :whitespace (pp-token-type val))
              ;; no change
              )
             (t (setf include-state 0)))
         ;; and assert that a header was found?
         )))))

;;(defrule c++-lex-phase67
;;  ;; combine phases 6 and 7?
;;  (

(defun parse-file (filename)
  (let (str
        (*filename* filename))
    (with-open-file (file filename)
      (setf str (make-sequence 'string (file-length file)))
      (read-sequence str file))
    (setf str (c++-lex-phase2 str))
    ;; eventually return the AST, the comment list, and the preproc list
    ;; also return a list of newlines (so line/col can be quickly calculated)
    ;; also return an indication if the parse didn't consume the whole file
    (multiple-value-bind (end val)
        (c++-lex-phase3b str)
      (unless (= end (length str))
        (warn "incomplete parse: ~a of ~a" end (length str)))
      val)))
