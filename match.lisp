(in-package :fn-impl)


;;;;;;
;;; Schema methods
;;;;;;

(defgeneric schema-match (schema pattern-args obj)
  (:documentation "Create a bindings object by doing pattern matching on obj.
  Return NIL if the matching fails."))
(defgeneric schema-pattern-vars (schema pattern-args)
  (:documentation "Get the list of symbols that would be bound by successful
  pattern matching on the given schema"))

(defmethod schema-match ((schema general-schema) pattern-args obj)
  (funcall (slot-value schema 'match) pattern-args obj))
(defmethod schema-pattern-vars ((schema general-schema) pattern-args)
  (aif (slot-value schema 'pattern-vars)
       (funcall it pattern-args)
       (mapcan #'schema-pattern-vars pattern-args)))

(defmethod schema-match ((schema data-schema) pattern-args obj)
  (when (eq (class-name (class-of obj)) ;check type first
            (slot-value schema 'name))
    (let ((x (destructure-arg-list (slot-value schema 'arg-list) pattern-args))
          (res {}))
      (block b
        (maphash $(aif (pattern-match $1 (@ $0 obj))
                       (setf res (dict-extend res it))
                       (return-from b nil))
                 x)
        res))))
(defmethod schema-pattern-vars ((schema data-schema) pattern-args)
  (let ((x (destructure-arg-list (slot-value schema 'arg-list)
                                 pattern-args))
        (res []))
    (maphash $(setq res (append res (pattern-vars $1))) x)
    res))


;;;;;;
;;; Pattern Matching
;;;;;;

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
          {}
          nil)
      (if (eql pattern obj)
          {}
          nil)))

(defun pattern-match (pattern obj)
  "Perform pattern matching and return either NIL (no match) or a bindings
object."
  (cond ((eq pattern '_) {})                              ;wildcard
        ((and (symbolp pattern) (not (keywordp pattern))) ;variable
         {pattern obj})
        ((is-literal pattern)           ; literal
         (literal-match pattern obj))
        ((listp pattern)                ; schema
         (aif (gethash (car pattern) schemas-by-name)
              (schema-match it (cdr pattern) obj)
              (progn (warn "Unknown pattern ~a" pattern)
                     nil)))
        (t (warn "Unknown pattern ~a" pattern)
           nil)))

(defun pattern-vars (pattern)
  "Makes a list of all variables that a pattern would bind."
  (cond ((eq pattern '_) nil)
        ((and (symbolp pattern)
              (not (keywordp pattern))
              (not (eq pattern '&)))
         (list pattern))
        ((is-literal pattern) nil)
        ((and (listp pattern) (symbolp (car pattern))) ;schema pattern
         (aif (gethash (car pattern) schemas-by-name)
              (mapcan #'pattern-vars (schema-pattern-vars it (cdr pattern)))
              (progn (warn "pattern-vars: schema not found ~s" (car pattern))
                     nil)))
        (t nil)))

(defun bindings-assignment (bindings vars)
  "Create a list of SETQ forms assigning the variables named in VARS to the
 corresponding values in BINDINGS. BINDINGS here is a symbol which names a local
 variable."
  (mapcar $`(setq ,$ (dict-get ,bindings ',$)) vars))


(defmacro my-case (obj &body clauses)
  "Perform pattern matching on OBJ. Each clause is a pattern and expression
pair. Patterns are tested in the order specified until the first match is found,
at which point the expression of the clause is executed."
  (let ((obj-var (gensym))
        (bid (gensym))
        (res (gensym)))
    `(let ((,obj-var ,obj))
       (block ,bid
         ,@(mapcar (lambda (x)
                     (let ((b (gensym))
                           (vars (pattern-vars (car x))))
                       `(let ((,b (pattern-match ',(car x) ,obj-var)))
                          (if ,b
                              (let (,res ,@vars)
                                ,@(bindings-assignment b vars)
                                (setq ,res ,(cadr x))
                                (return-from ,bid ,res))))))
                   (group 2 clauses))))))


;;;;;;
;;; Pre-defined Schemas
;;;;;;


;; add the list schema
(add-schema
 (make-instance
  'general-schema
  :name 'list
  :data-classes '(cons list null)
  :construct #'list
  :get (lambda (instance slot)
         (declare (type integer slot)
                  (type list instance))
         (nth slot instance))
  :set (lambda (instance slot value)
         (declare (type list instance)
                  (type integer slot))
         (setf (nth slot instance) value))
  :match (lambda (pattern-args obj)
           (when (listp obj)
             (rlambda (res a* x*) ({}  pattern-args obj)
               (cond ((null a*) (if (null x*)
                                    res
                                    nil))
                     ((eq (car a*) '&)
                      (unless (eq (length a*) 2)
                        (error "list matching: wrong arguments after &~s"
                               (cdr a*)))
                      (aif (pattern-match (cadr a*) x*)
                           (dict-extend res it)
                           nil))
                     ((null x*) nil)
                     (t (aif (pattern-match (car a*) (car x*))
                             (recur (dict-extend res it)
                                    (cdr a*)
                                    (cdr x*))
                             nil))))))
  :pattern-vars (lambda (pattern-args)
                  (mapcan #'pattern-vars
                          (remove-if $(eq $ '&) pattern-args)))))


(add-schema
 (make-instance
  'general-schema
  :name 'dict
  :data-classes '(hash-table)
  :construct #'dict
  :get (lambda (instance slot)
         (declare (type dict instance))
         (dict-get instance slot))
  :set (lambda (instance slot value)
         (declare (type dict instance))
         (setf (dict-get instance slot) value))
  :match (lambda (pattern-args obj)
           (when (is-dict obj)
             ;; this generates an error when there are illegal args
             (let ((x (apply #'dict pattern-args))
                   (res {}))
               (block b
                 (maphash $(aif (pattern-match $1 (dict-get obj $0))
                                (setq res (dict-extend res it))
                                (return-from b nil))
                          x)
                 res))))
  :pattern-vars (lambda (pattern-args)
                  (let ((pairs (group 2 pattern-args)))
                    (mapcan $(pattern-vars (cadr $)) pairs)))))

;; dict pattern is
;; (dict KEYFORM PATTERN KEYFORM PATTERN ...)
;; KEYFORM := KEY | (KEY DEFAULT-VALUE)
;; recommended to use :keywords for dict schema
