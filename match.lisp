(in-package :fn-impl)

(defclass bindings ()
  ((alist :initarg :alist
          :initform (list)
          :type list
          :accessor bindings-alist
          :documentation "An ALIST of symbols and values."))

  (:documentation "An object representing a set of variable bindings."))

(defun bindings-conc (b1 &rest b2)
  "Non-destructively concatenate two bindings objects."
  (if b2
      (let ((b* (append (bindings-alist b1)
                        (bindings-alist (apply #'bindings-conc b2)))))
        (make-instance 'bindings :alist b*))
      b1))

(defun bindings-assignment (bindings vars)
  (mapcar (lambda (x)
            `(setq ,x (cdr (assoc ',x (bindings-alist ,bindings)))))
          vars))


(defun is-quoted (pattern)
  "Tell if pattern is a quoted form."
  (and (listp pattern)
       (eq (car pattern) 'quote)))

(defun is-literal (pattern)
  "Tell if pattern is a literal."
  (or (numberp pattern)
      (characterp pattern)
      (stringp pattern)
      (is-quoted pattern)))

(defun literal-match (pattern obj)
  "Match a literal pattern."
  (if (is-quoted pattern)
      (if (eq (cadr pattern) obj)
          (make-instance 'bindings)
          nil)
      (if (eql pattern obj)
          (make-instance 'bindings)
          nil)))

(defun pattern-match (pattern obj)
  (cond ((eq pattern '_)                ; wildcard
         (make-instance 'bindings))
        ((and (symbolp pattern) (not (keywordp pattern))) ; variable
         (make-instance 'bindings
                        :alist (list (cons pattern obj))))
        ((is-literal pattern)           ; literal
         (literal-match pattern obj))
        ((listp pattern)                ; schema
         (aif (gethash (car pattern) schemas-by-name)
              (and (some $(subtypep (class-name (class-of obj))
                                    $)
                         (slot-value it 'data-classes))
                   (schema-match it (cdr pattern) obj))
              nil))
        (t (warn "Unknown pattern ~a" pattern)
           nil)))

;;; FIXME: this would not work with general schemas. In the future, we will need
;;; another schema method that gives the names of the fields bound by that
;;; schema.
(defun pattern-vars (pattern)
  "Makes a list of all variables that a pattern would bind."
  (cond ((eq pattern '_) nil)
        ((and (symbolp pattern)
              (not (keywordp pattern))
              (not (eq pattern '&)))
         (list pattern))
        ((is-literal pattern) nil)
        ((listp pattern)
         (mapcan #'pattern-vars (cdr pattern)))
        (t nil)))

(defmacro match (obj &body patterns-and-clauses)
  (let ((obj-var (gensym)))
    `(let ((,obj-var ,obj))
       (or ,@(mapcar (lambda (x)
                       (let ((b (gensym))
                             (vars (pattern-vars (car x))))
                         `(let ((,b (pattern-match ',(car x) ,obj-var)))
                            (if ,b
                                (let ,vars
                                  ,@(bindings-assignment b vars)
                                  ,(cadr x))))))
                     (group 2 patterns-and-clauses))))))

