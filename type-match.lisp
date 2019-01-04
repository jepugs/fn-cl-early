(load "type-parse.lisp")

(defun match-normal-types (type1 type2 &optional t1-vars t2-vars)
  (labels
      ((resolv1 (x)
         (cdr (assoc x t1-vars)))
       (resolv2 (x)
         (cdr (assoc x t2-vars)))

       (check-match (a1 a2)
         (cond
           ((symbolp a1)
            (aif (resolv1 a1)
                 (type= it a2)
                 (progn (push (cons a1 a2) t1-vars) t)))
           ((symbolp a2)
            (aif (resolv2 a2)
                 (type= it a1)
                 (progn (push (cons a1 a2) t1-vars) t)))
           ((is-normal-type a1)
            (if (is-normal-type a2)
                (destructuring-bind (res vars1 vars2)
                    (match-normal-types a1 a2 t1-vars t2-vars)
                  (if res
                      (progn (setq t1-vars (nconc vars1 t1-vars))
                             (setq t2-vars (nconc vars2 t2-vars))
                             t)
                      nil))))))
       (iterate (args1 args2)
         (cond
           ((and args1 args2)
            (check-match (car args1) (car args2))
            (iterate (cdr args1) (cdr args2)))
           ((or args1 args2) nil)   ; different arg lengths
           (t (list t t1-vars t2-vars)))))
    (if (eq (tcons-of type1) (tcons-of type2))
        (iterate (slot-value type1 'args)
                 (slot-value type2 'args))
        nil)))

