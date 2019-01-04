(load "type-defs.lisp")

(defun parse-fn-args (type)
  "Get a list of argument types."
  (if (eq (car type) '->)
      nil
      (cons (car type) (fn-args (cdr type)))))

(defun parse-fn-res (type)
  "Get the result type from a function."
  (if (eq (car type) '->)
      (cadr type)
      (fn-res (cdr type))))

(defun parse-type (x)
  (cond ((symbolp x) x)
        ((listp x) 
         (if (member '-> x)
             (make-instance 'fn-type
                            :args (mapcar #'parse-type (parse-fn-args x))
                            :res (parse-fn-res x))
             (make-instance 'normal-type
                            :tcons (car x)
                            :args (mapcar #'parse-type (cdr x)))))
        (t (error "couldn't parse type, ya dingus"))))

