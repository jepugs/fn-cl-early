(in-package :fn-impl)

(defun convert-fn-llist (l)
  (let (pos opt key rest)
    ;; FIXME: this would probably be better expressed with LOOP
    (labels ((iter (tl pos-p)           ;process the argument list
               ;; this is probably a mistake and can yield some whacky behavior
               (when (member (car tl) '(&rest &whole &body &key &optional))
                 (warn "FN lambda lists don't understand ~a arguments~%"
                       (car tl)))
               (cond
                 ((not tl) nil)
                 ((eq (car tl) '&)      ;varargs
                  (if (= (length tl) 2) ;must be the last element
                      (setq rest (cadr tl))
                      (error "Malformed FN llambda list")))
                 ((and (symbolp (car tl)) pos-p) ;positional argument
                  (push (car tl) pos)
                  (iter (cdr tl) pos-p))
                 ((and (listp (car tl)) (keywordp (caar tl))) ;key argument
                  (push (car tl) key)
                  (iter (cdr tl) nil))
                 ((and (listp (car tl)) (symbolp (caar tl))) ;optional arg
                  (push (car tl) opt)
                  (iter (cdr tl) nil))
                 (t (error "Malformed FN lambda list"))))
             (fix-key (keyform)         ;make key arguments suit CL
               (let ((name (intern (symbol-name (car keyform)))))
                 (cons name (cdr keyform)))))
      (iter l t)
      ;; build a normal lambda list
      (nconc (nreverse pos)
             (if opt '(&optional))
             (nreverse opt)
             (if key '(&key))
             (mapcar #'fix-key (nreverse key))
             (if rest
                 ['&rest rest])))))

(defmacro fn (llist &body body)
  `(lambda ,(convert-fn-llist llist) . ,body))

(defmacro defn (llist &body body)
  `(defun ,(convert-fn-llist llist) . ,body))
