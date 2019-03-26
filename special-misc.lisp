(in-package :fn-impl)

(defmacro my-do (&body body)
  `(cl:progn ,@body))

(defmacro my-let (bindings &body body)
  (let* ((pairs (group 2 bindings))
         (patterns (mapcar #'car pairs))
         (objs (mapcar #'cadr pairs)))
    (pattern-assignments patterns
                         objs
                         `(progn ,@body)
                         `(error "let: match failed"))))
