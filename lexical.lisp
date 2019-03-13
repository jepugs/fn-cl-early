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
;;; with that name that expands to (get SYM 'lexically-bound). This works
;;; because local lexical variables (i.e. ones created by let, lambda, etc) will shadow 


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

(defmacro define-lexically (sym val mutable)
  ;; Raise a warning if we redefine a variable with a different initform
  (if (and (is-lexical sym)
           (not mutable)
           (not (is-mutable sym))
           (not (equal (get sym 'initform) val)))
      (warn "DEF: rebinding constant ~a with new initform." sym))
  (let ((gval (gensym)))
    (unless (symbolp sym)
      (error "DEF: var is not a symbol ~a" sym))
    `(progn
       (when (is-dynamic ',sym)
         (error "DEF: ~a is already dynamically bound" ',sym))
       (let ((,gval ,val))
         (setf (get ',sym 'lexical-binding) ,gval)
         (setf (get ',sym 'lexically-bound) t)
         (setf (get ',sym 'initform) mutable)
         (setf (get ',sym 'mutable) mutable)
         (when (functionp ,gval)
           (setf (symbol-function ',sym) ,gval))
         (define-symbol-macro ,sym (get ',sym 'lexical-binding))))))

(defun set-variable (sym val)
  "Update the value of a variable."
  (cond ((is-lexical sym)
         (Unless (is-mutable sym)
           (error "set: ~a is immutable" sym))
         (setf (get sym 'lexical-binding) val)
         (if (functionp val)
             (setf (symbol-function sym) val)
             (fmakunbound sym)))
        (t (set sym val))))

(defmacro def (&body pattern-value-pairs)
  "Define a lexical constant."
  (let* ((pairs (group 2 pattern-value-pairs)))
    `(progn . ,(mapcar $`(define-lexically ,(car $) ,(cadr $) nil)
                       pairs))))

(defmacro my-defvar (&body pattern-value-pairs)
  "Define a lexical variable."
  ;; same as def except we set the mutable flag to T
  (let* ((pairs (group 2 pattern-value-pairs)))
    `(progn . ,(mapcar $`(define-lexically ,(car $) ,(cadr $) t)
                       pairs))))

(defmacro defn (name arg-list &body body)
  `(def ,name (fn ,arg-list . ,body)))
