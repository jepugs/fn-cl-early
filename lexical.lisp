;;;; lexical.lisp -- implementation of lexical variables and the def special
;;;; form

(in-package :fn-impl)


;;;; Implementation of global lexical variables
;;; We will use the symbol indicator 'lexically-bound to tell whether a symbol
;;; has a lexical value and 'lexical-binding to hold the actual value. In
;;; addition, to simulate a LISP-1, whenever a lexical variable is set to a
;;; function value, the corresponding symbol-function is set as well.
;;; Conversely, when symbol is changed from a function to a non-function, we
;;; must unbind the symbol.
;;;
;;; The final step in defining a lexical variable is to create a symbol macro
;;; with that name that expands to (lexical-value SYM). This works because local
;;; lexical variables (i.e. ones created by let, lambda, etc) will shadow the
;;; symbol macro, giving us appropriate lexical scope.
;;;
;;; In order to manage variables vs constants, we also attach a boolean called
;;; 'mutable to the associated variable. In our SETF function, we check this
;;; flag and raise an error if appropriate. Every variable defined with DEF is
;;; immutable, while variables defined with MY-DEFVAR are mutable.


(defun is-mutable (sym)
  "Tell if SYM is mutable"
  (get sym 'mutable))

(defun is-lexical (sym)
  "Tell if SYM has a global lexical binding."
  (get sym 'lexically-bound))

(defun is-dynamic (sym)
  "Tell if SYM has a global dynamic binding."
  (boundp sym))

(defun sym-initform (sym)
  "Get the form used to initialize the value of sym"
  (get sym 'initform))

(defun lexical-value (sym)
  "Get the (global) lexical value of sym"
  (get sym 'lexical-binding))

(defun (setf lexical-value) (val sym)
  (cond
    ((not (is-lexical sym))
     (error "Attempt to set unbound global lexical symbol ~a" sym))
    ((not (is-mutable sym))
     (error "Attempt to set the immutable variable ~a" sym))
    (t
     (setf (get sym 'lexical-binding) val)
     (if (functionp val)
             (setf (symbol-function sym) val)
             (fmakunbound sym)))))

(defmacro define-lexically (sym val mutable)
  ;; Raise a warning if we redefine a variable with a different initform
  (if (and (is-lexical sym)
           (not mutable)
           (not (is-mutable sym))
           (not (equal (get sym 'initform) val)))
      nil
      ;;(warn "DEF: rebinding constant ~a with new initform." sym)
      )
  (let ((gval (gensym)))
    (unless (symbolp sym)
      (error "DEF: var is not a symbol ~a" sym))
    `(progn
       (when (is-dynamic ',sym)
         (error "DEF: ~a is already dynamically bound" ',sym))
       (let ((,gval ,val))
         (setf (get ',sym 'lexical-binding) ,gval)
         (setf (get ',sym 'lexically-bound) t)
         (setf (get ',sym 'initform) ',val)
         (setf (get ',sym 'mutable) ,mutable)
         (when (functionp ,gval)
           (setf (symbol-function ',sym) ,gval))
         (define-symbol-macro ,sym (lexical-value ',sym))))))

(defun set-variable (sym val)
  "Update the value of a variable."
  (cond ((is-lexical sym)
         (unless (is-mutable sym)
           (error "set: ~a is immutable" sym))
         (setf (get sym 'lexical-binding) val)
         (if (functionp val)
             (setf (symbol-function sym) val)
             (fmakunbound sym)))
        (t (set sym val))))

