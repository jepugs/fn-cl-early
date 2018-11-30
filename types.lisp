;;;; types.lisp

;; FIXME: use packages instead of this mess
(load "macros.lisp")

;;; Conditions
(define-condition type-mismatch-error (error)
  ((expected :initarg :expected
             :type (or list symbol)
             :documentation "The type which was expected")
   (actual :initarg :actual
           :type (or list symbol)
           :documentation "The type encountered instead")
   (obj :initarg :obj
        :initform nil
        :documentation "The offending object (if available)")
   (obj-p :initarg :obj-p
          :initform nil
          :type boolean
          :documentation "Whether OBJ is defined")
   (context :initarg :context
            :initform nil
            :documentation "Optional expression where the error occurred"))
  (:documentation "Indicates that a type check failed"))

(define-condition unknown-type-error (error)
  ((expr :initarg :expr
         :type (or list symbol)
         :documentation "The errant type"))
  (:documentation "Indicates that an unknown type constructor was encountered"))

(define-condition malformed-type-error (error)
  ((expr :initarg :expr
         :type (or list symbol)
         :documentation "The invalid type expression")
   (desc :initarg :desc
         :type string
         :initform ""
         :documentation "Description of the error"))
  (:documentation "Indicates that a type is not syntactically valid"))

(defun ut-error (expr)
  (error 'unknown-type-error :expr expr))

(defun mft-error (expr &rest args)
  (apply #'error 'malformed-type-error :expr expr args))

;;; Basic operations on types
(defun is-quoted-t (type)
  "True iff TYPE is a quoted type"
  (and (is-quoted type) (symbolp (cadr type))))
(defun is-wild-t (type)
  "True iff TYPE is a wildcard"
  (eq type '_))
(defun is-var-t (type)
  "True iff TYPE is a variable"
  (and (symbolp type) (not (is-wild-t type))))
(defun is-fn-t (type)
  "True iff TYPE is a function type"
  (and (listp type) (member '-> type)))
(defun tcons-of (type)
  "Get the type constructor of TYPE"
  (cond ((is-wild-t type) '_)
        ((is-quoted-t type) (cadr type))
        ((is-fn-t type) '->)
        ((listp type) (car type))
        (t nil)))

(defparameter illegal-tcons '(quote nil & t)
  "Things you can't name your TCONS")
(defparameter special-tcons '(-> or number char bool symbol list)
  "TCONS which have special matching functions. These include wildcard and
primitive types.")
(defparameter primitive-types '(number char bool symbol))

(defun is-legal-tcons (x)
  (and (symbolp x)
       (not (member x illegal-tcons))
       (not (keywordp x))))

(defun is-special-t (x)
  (member (tcons-of x) special-tcons))

(defun is-prim-t (x)
  (member (tcons-of x) primitive-types))

(defun check-type-syntax (type)
  "Check type syntax. Signals MALFORMED-TYPE-ERROR if the type is not valid."
  (cond ((listp type)
         (if (member '-> type)
             (mapcar #'check-type-syntax (remove-if $(eq $ '->) type))
             (mapcar #'check-type-syntax (cdr type)))
         (unless (is-legal-tcons (tcons-of type))
               (mft-error type :desc "Illegal type constructor"))
         t)
        ((is-wild-t type) t)
        ((is-var-t type) t)
        (t (mft-error type :desc "Illegal type"))))

(defun fn-args (type)
  "Get a list of argument types."
  (if (eq (car type) '->)
      nil
      (cons (car type) (fn-args (cdr type)))))

(defun fn-res (type)
  "Get the result type from a function."
  (if (eq (car type) '->)
      (cadr type)
      (fn-res (cdr type))))


;;; subtype tests

;;;; BIGFIXME: I'm pretty sure all these binding combination functions are very,
;;;; very broken. We need to work this one out on a whiteboard.
(defun combine-binding (sym x y)
  "Helper for PAIRWISE-COALESCE. Attempts to combine X and Y into a single
binding with name SYM and returns an ALIST of variable bindings including SYM.
Returns NIL on failure."
  (let ((xy (is-subtype x y)))
    (if xy
        `((,sym . ,y) . ,(cdr xy))
        (let ((yx (is-subtype y x)))
          (if yx
              `((,sym . ,x) . ,(cdr yx))
              nil)))))

(defun pairwise-coalesce (x y)
  "Helper for COALESCE-BINDINGS."
  (cons
   t
   (if (null x)
       y
       (let ((syms (remove-duplicates (append (mapcar #'car x)
                                              (mapcar #'car y))))
             res)
         (loop for s in syms
            for bx = (assoc s x)
            for by = (assoc s y)
            do (cond ((null bx) (push by res))
                     ((null by) (push bx res))
                     (t (let ((c (combine-binding s (cdr bx) (cdr by))))
                          (if c
                              (setq res (nconc c res))
                              (return nil))))))
         (return (nreverse res))))))

(defun coalesce-bindings (alists)
  "Attempt to combine a bunch of type variable bindings. Returns (T . bindings)
on success and NIL on failure."
  (declare (ignore alists))
  nil)

;;;; BIGFIXME: most of the subtype tests below still don't return the bindings
;;;; on success, which causes a lot of trouble.
(defun or-subtype (sub or-args)
  (if (eq (tcons-of sub) 'or)
      (every $(or-subtype $ or-args) (cdr sub))
      (some $(is-subtype sub $) or-args)))
(defun or-supertype (or-args super)
  (every $(is-subtype $ super) or-args))

(defun fn-subtype (sub-args sup-args sub-res sup-res)
  (if (= (length sub-args) (length sup-args))
      (let ((arg-bs (mapcar #'is-subtype sub-args sup-args))
            (res-bs (is-subtype sub-res sup-res)))
        (if (and (car res-bs)
                 (every #'car arg-bs))
            (coalesce-bindings (mapcar #'cdr (cons res-bs arg-bs)))
            nil))
      nil))

(defun special-subtype (sub super)
  (let ((subt (tcons-of sub))
        (supt (tcons-of super)))
    (case supt
      ((or) (or-subtype sub (cdr super)))
      ((->)
       (and (is-fn-t sub)
            (fn-subtype (fn-args sub) (fn-args super)
                        (fn-res sub) (fn-res super))))
      ;; primitive types
      ((number char bool) (eq subt supt))
      ((symbol) (member subt '(symbol bool))))))

(defun is-subtype (sub super)
  "Decide if SUB is a valid subtype of SUPER. Returns NIL if false and (T .
bindings) if true, where bindings is an ALIST of variables in SUPER."
  (cond ((is-var-t super) `(t . (,super . ,sub)))
        ((or (is-wild-t super)
             (is-var-t sub))
         (cons t nil))
        ;; OR is so weird that it needs special treatment even as a subtype.
        ((eq (tcons-of sub) 'or) (or-supertype (cdr sub) super))
        ((is-special-t super)
         (special-subtype sub super))
        ((is-prim-t super) (prim-subtype sub super))
        (t (and (eq (tcons-of sub) (tcons-of super))
                (mapcar #'is-subtype (cdr sub) (cdr super))))))
