;;;; eval.lisp -- expression evaluator

;;;; This file is part of fn.

;;;; fn is free software: you can redistribute it and/or modify it under the terms of the GNU
;;;; General Public License as published by the Free Software Foundation, either version 3 of the
;;;; License, or (at your option) any later version.

;;;; fn is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even
;;;; the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;;;; General Public License for more details.

;;;; You should have received a copy of the GNU General Public License along with fn. If not, see
;;;; <https://www.gnu.org/licenses/>.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defpackage :fn.eval
  (:documentation "expression evaluator")
  (:use :cl :fn.util :fn.ast :fn.values)
  (:export :eval-ast :eval-code))

(in-package :fn.eval)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; code intermediate representation

;;; ASTs are converted to code objects before evaluation.

;;; - numbers and strings are converted to fn values
;;; - symbols are interned
;;; - brackets, dots, quotes, etc are expanded into homoiconic command form

;;; symbol names used during syntax tree expansion
(defconstant-1 bracket-sym-name "List")
(defconstant-1 brace-sym-name "Table")
(defconstant-1 dollar-sym-name "dollar-fn")
(defconstant-1 dot-sym-name "@")
(defconstant-1 quot-sym-name "quote")

;;; Each Code object retains a reference to the AST object from which it was derived (for error
;;; handling).

(defstruct (code (:constructor make-code (src data))
                 (:predicate code?))
  ;; the AST object that this code was derived from (if any)
  src
  ;; the converted ast object. This may be either an fn value or a list.
  data)

(defun code-intern (src name symtab)
  "Like SYMTAB-INTERN but wraps the generated symbol in a code object."
  (make-code src (symtab-intern name symtab)))

(defun ast->code (a symtab)
  "Converts an AST object to code."
  (make-code
   a
   (cond
     ((ast-string? a) (fnstring (slot-value a 'value)))
     ((ast-number? a) (num (slot-value a 'value)))

     ((ast-paren? a) (mapcar $(ast->code $ symtab)
                             (slot-value a 'contents)))
     ((ast-bracket? a) (cons (symtab-intern bracket-sym-name symtab)
                             (mapcar $(ast->code $ symtab)
                                     (slot-value a 'contents))))
     ((ast-brace? a) (cons (symtab-intern brace-sym-name symtab)
                           (mapcar $(ast->code $ symtab)
                                   (slot-value a 'contents))))

     ((ast-quot? a) (list (symtab-intern quot-sym-name symtab)
                          (ast->code (slot-value a 'expr) symtab)))

     ((or (ast-quasiquot? a)
          (ast-unquot? a)
          (ast-unquot-splice? a))
      (fn-error "FN.CODE"
                "Quasiquotation is not implemented"
                (slot-value a 'line)))

     ((ast-dollar? a) (list (symtab-intern dollar-sym-name symtab)
                            (ast->code (slot-value a 'expr) symtab)))

     ((ast-dot? a) (list (symtab-intern dot-sym-name symtab)
                         (ast->code (slot-value a 'left) symtab)
                         (ast->code (slot-value a 'right) symtab)))

     ((ast-sym? a) (symtab-intern (slot-value a 'name) symtab)))))

(defun code-list? (c)
  "Tell if a code object contains a list."
  (listp (code-data c)))

(defun code-sym? (c)
  (sym? (code-data c)))
(defun code-sym-name (c)
  (sym-name (code-data c)))
(defun code-sym-id (c)
  (sym-id (code-data c)))

(defun code-literal? (c)
  (with-slots (data) c
    (or (string? data) (num? data))))

(defun code-op-is (c op-name)
  "Check whether C has a certain symbol in its operator position. Assumes C contains a list."
  (with-slots (data) c
    (and (code-sym? (car data))
         (string= (code-sym-name (car data)) op-name))))

(eval-when (:load-toplevel :compile-toplevel :execute)
  (defmacro code-error (c format-string &rest format-args)
    `(fn-error ,(package-name *package*)
               (format nil ,format-string ,@format-args)
               (slot-value (slot-value ,c 'src) 'line))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; special operator creation macros

;;; macros for to make defining special operators and syntax checking more convenient

(eval-when (:load-toplevel :compile-toplevel :execute)

  (defparameter special-op-dispatch nil)

  (defmacro def-op (name arg-specs &body body)
    "DEF-OP defines a special operator in fn by adding it to SPECIAL-OP-DISPATCH.

 NAME : a string denoting the operator's name
 ARG-SPECS : a list describing the operator's arguments (see below)
 BODY : function body describing the operator's behavior

 All the names in the ARG-SPECs list are bound to corresponding arguments for use in the function
 body. In addition, variables named C, ENV, and INTERPRETER are bound holding evaluation
 parameters (C is the code object for the current expression)."
    `(push
      (cons ,name
            (lambda (c env interpreter)
              (let ,(arg-spec-vars arg-specs)
                ,(arg-binders 'c arg-specs name)
                ,@body)))
      special-op-dispatch))

  (defun arg-spec-vars (arg-specs)
    "Get a list of variables bound by def-op arg-specs"
    (reduce $(cond ((listp $1) (cons (car $1) $0))
                   ((eq $1 '&) $0)
                   (t (cons $1 $0)))
            arg-specs
            :initial-value nil))

  (defun arg-binders (code arg-specs op-name)
    "Based upon arg-specs, generate code to do argument validation and binding for def-op"
    (with-gensyms (x)
      `(let ((,x (cdr (code-data ,code))))
         ,@(rlambda (acc specs) (nil arg-specs)
             (cond
               ((null specs)
                `(,@(nreverse acc)
                  (unless (null ,x)
                    (code-error ,code "Too many arguments for ~a" ,op-name))))
               ((eq (car specs) '&)
                `(,@(nreverse acc)
                  ,(if (listp (cadr specs))
                       `(progn (funcall ,(cadadr specs) ,x)
                               (setq ,(caadr specs) ,x))
                       `(setq ,(cadr specs) ,x))))
               ((symbolp (car specs))
                (recur
                 `((if (null ,x)
                       (code-error ,code "Not enough arguments for ~a" ,op-name)
                       (progn (setq ,(car specs) (car ,x))
                              (setq ,x (cdr ,x))))
                   ,@acc)
                 (cdr specs)))
               ((listp (car specs))
                (recur
                 `((if (null ,x)
                       (code-error ,code "Not enough arguments for ~a" ,op-name)
                       (progn
                         (funcall ,(cadar specs) (car ,x))
                         (setq ,(caar specs) (car ,x))
                         (setq ,x (cdr ,x))))
                   ,@acc)
                 (cdr specs)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; main interpreter code

(defstruct interpreter
  (globals (init-env nil))
  (macros (init-env nil))
  (symtab (make-instance 'symtab)))

(defun fnintern (name interpreter)
  "Get an internal symbol using INTERPRETER's symbol table."
  (symtab-intern name (interpreter-symtab interpreter)))

(defparameter *global-interpreter* (make-interpreter))

(defun eval-code (c &optional (env (init-env nil)) (interpreter *global-interpreter*))
  "Evaluate a code object."
  (cond
    ((code-literal? c) (code-data c))
    ((code-sym? c) (eval-var c env interpreter))
    ((code-list? c) (eval-command c env interpreter))
    (t (fn-error "FN.EVAL"
                 "invalid code object passed to EVAL-CODE"))))

(defun eval-ast (a &optional (env (init-env nil)) (interpreter *global-interpreter*))
  "Evaluate an AST object."
  (with-slots (symtab) interpreter
    (eval-code (ast->code a symtab) env interpreter)))

(defun find-cell (sym env interpreter)
  "Find a value cell corresponding to a symbol."
  (or (env-get env sym)
      (gethash (sym-id sym) (slot-value interpreter 'globals))))

(defun eval-var (c env interpreter)
  "Evaluate a variable reference. Assumes CODE contains a symbol"
  (aif (find-cell (code-data c) env interpreter)
       (cell-value it)
       (code-error c "Unbound symbol: ~a" (code-sym-name c))))

(defun eval-command (c env interpreter)
  (let ((op (car (code-data c))))
    (aif (and (code-sym? op)
              (assoc (code-sym-name op)
                     special-op-dispatch
                     :test #'equal))
         (funcall (cdr it) c env interpreter)
         nil)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; special operators

(defun validate-def-name (c)
  (unless (or (code-sym? c)
              (and (code-list? c)
                   (code-sym? (car (code-data c)))))
    (code-error c "Illegal name in definition")))

(def-op "def" ((name #'validate-def-name) & value-or-body)
  (if (code-list? name)
      (eval-function-def name value-or-body env interpreter)
      (if (length= value-or-body 1)
          (let ((v (eval-code (car value-or-body))))
            (env-add (slot-value interpreter 'globals)
                     (code-data name)
                     (make-cell :value v :mutable nil))
            v)
          (code-error c "Too many arguments in definition"))))

(def-op "defvar" ((name #'validate-def-name) & value-or-body)
  (if (code-list? name)
      (eval-function-def name value-or-body env interpreter)
      (if (length= value-or-body 1)
          (let ((v (eval-code (car value-or-body))))
            (env-add (slot-value interpreter 'globals)
                     (code-data name)
                     (make-cell :value v :mutable t))
            v)
          (code-error c "Too many arguments in definition"))))

(defun validate-set-place (c)
  (unless (code-sym? c)
    (code-error c "Illegal place in set expression")))

(def-op "set" ((name #'validate-set-place) value)
  (let ((cell (find-cell (code-data name) env interpreter)))
    (cond
      ((null cell)
       (code-error c "Undefined variable ~s" (code-sym-name name)))

      ((not (cell-mutable cell))
       (code-error c
                   "Attempt to set immutable variable ~s"
                   (code-sym-name name)))

      (t (cell-mutable cell)
         (setf (cell-value cell)
               (eval-code value env interpreter))))))


