;;;; lexical.lisp -- implementation of lexical variables and the def special
;;;; form

(load "package-impl.lisp")
(load "macros.lisp")
(load "match.lisp")
(load "schema.lisp")

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


(defun is-lexical (sym)
  "Tell if SYM has a global lexical binding."
  (get sym 'lexically-bound))

(defun is-dynamic (sym)
  "Tell if SYM has a global dynamic binding."
  (boundp sym))

(defmacro define-lexically (sym val)
  (let ((gval (gensym)))
    (unless (symbolp sym)
      (error "DEF: var is not a symbol ~a" sym))
    `(progn
       (when (is-dynamic ',sym)
         (error "DEF: ~a is already dynamically bound" ',sym))
       (let ((,gval ,val))
         (setf (get ',sym 'lexical-binding) ,gval)
         (setf (get ',sym 'lexically-bound) t)
         (when (functionp ,gval)
           (setf (symbol-function ',sym) ,gval))
         (define-symbol-macro ,sym (get ',sym 'lexical-binding))))))

(defun set-variable (sym val)
  "Update the value of a variable."
  (cond ((is-lexical sym)
         (setf (get sym 'lexical-binding) val)
         (if (functionp val)
             (setf (symbol-function sym) val)
             (fmakunbound sym)))
        (t (set sym val))))

;; TODO: pattern matching & multiple definitions my dude
(defmacro def (&body pattern-value-pairs)
  "Define a lexical variable."
  (let* ((pairs (group 2 pattern-value-pairs)))
    `(progn . ,(mapcar (lambda (x)
                         (match x
                           [n v] `(define-lexically ,n ,v)))
                       pairs))))


