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
        (maphash $(aif (pattern-match $1 (schema-get schema obj $0))
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
      (if (equal (cadr pattern) obj)
          {}
          nil)
      (if (equal pattern obj)
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

(defun patterns-match (patterns objs)
  "Chain together a series of pattern matches."
  (let ((match-res (mapcar #'pattern-match patterns objs)))
    (if (every #'values match-res)
        (reduce #'dict-extend
                match-res
                :initial-value {})
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

(defun for-match-vars (pattern obj body-func)
  "For use in macro writing. Returns code that executes body for each variable
 bounding by the match. Body func should accept two arguments, a name var and a
 form var, and return an expression for the body (i.e. code to execute for each
 pair)."
  (let ((b (gensym)))
    `(let ((,b (pattern-match ',pattern ,obj)))
       ,@(mapcar (lambda (x)
                   (funcall body-func x `(dict-get ,b ',x)))
                 (pattern-vars pattern)))))

(defun bindings-assignment (bindings vars)
  "Create a list of SETQ forms assigning the variables named in VARS to the
 corresponding values in BINDINGS. BINDINGS here is a symbol which names a local
 variable."
  (mapcar $`(setq ,$ (dict-get ,bindings ',$)) vars))

(defun pattern-assignments (patterns objs match-form else-form)
  "Creates code to declare and assign the variables in patterns to their matches
 in objs. This binds both variable and function namespaces. If all matches are
 successful, match-form is evaluated in the new lexical environment. Otherwise,
 else-form is evaluated."
  (let ((b (gensym))
        (vars (mapcan #'pattern-vars patterns)))
    `(let ((,b (patterns-match ',patterns [,@objs])))
       (if ,b
           (let ,vars
             (labels ,(mapcar $(let ((g (gensym)))
                                 `(,$ (&rest ,g)
                                      (cl:apply ,$ ,g)))
                              vars)
               (progn ,@(bindings-assignment b vars))
               ,match-form))
           ,else-form))))



