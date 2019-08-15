;;;; code.lisp -- intermediate representation and syntax checking

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

(defpackage :fn.code
  (:documentation "intermediate representation and syntax checking")
  (:use :cl :fn.util :fn.test :fn.ast :fn.values)
  (:export :code :origin :data :make-code :code-origin :code-data :code? :code-intern :ast->code
           :code-list? :code-sym? :code-sym-name :code-sym-id :code-car :code-cadr :code-cdr
           :code-literal? :code-quoted-sym? :code-op-is :code->fnvalue :code->fnvalue :validate-code
           :bracket-sym-name :brace-sym-name :dollar-sym-name :dot-sym-name :quot-sym-name
           :quasiquot-sym-name :unquot-sym-name :unquot-splice-sym-name :true-sym-name
           :false-sym-name :null-sym-name))

(in-package :fn.code)

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
(defconstant-1 dot-sym-name "get")
(defconstant-1 quot-sym-name "quote")
(defconstant-1 quasiquot-sym-name "quasiquote")
(defconstant-1 unquot-sym-name "unquote")
(defconstant-1 unquot-splice-sym-name "unquote-splice")
(defconstant-1 true-sym-name "true")
(defconstant-1 false-sym-name "false")
(defconstant-1 null-sym-name "null")

;;; Each Code object retains a reference to the AST object from which it was derived (for error
;;; handling).

(defstruct (code (:constructor make-code (origin data))
                 (:predicate code?))
  ;; this may be an AST object or a macro
  origin
  ;; the converted ast object. This may be either an fn value or a list.
  data)

(defun code-intern (origin name symtab)
  "Like SYMTAB-INTERN but wraps the generated symbol in a code object."
  (make-code origin (symtab-intern name symtab)))

(defun ast->code (a symtab)
  "Converts an AST object to code."
  (let ((o (slot-value a 'origin)))
    (make-code
     o
     (cond
       ((ast-string? a) (fnstring (slot-value a 'value)))
       ((ast-number? a) (num (slot-value a 'value)))

       ((ast-paren? a)  (mapcar $(ast->code $ symtab)
                                (slot-value a 'contents)))
       ((ast-bracket? a) (cons (code-intern o bracket-sym-name symtab)
                               (mapcar $(ast->code $ symtab)
                                       (slot-value a 'contents))))
       ((ast-brace? a) (cons (code-intern o brace-sym-name symtab)
                             (mapcar $(ast->code $ symtab)
                                     (slot-value a 'contents))))

       ((ast-quot? a) (list (code-intern o quot-sym-name symtab)
                            (ast->code (slot-value a 'expr) symtab)))
       ((ast-quasiquot? a) (list (code-intern o quasiquot-sym-name symtab)
                                 (ast->code (slot-value a 'expr) symtab)))
       ((ast-unquot? a) (list (code-intern o unquot-sym-name symtab)
                              (ast->code (slot-value a 'expr) symtab)))
       ((ast-unquot-splice? a) (list (code-intern o unquot-splice-sym-name symtab)
                                     (ast->code (slot-value a 'expr) symtab)))

       ((ast-dollar? a) (list (code-intern o dollar-sym-name symtab)
                              (ast->code (slot-value a 'expr) symtab)))

       ((ast-dot? a) (list (code-intern o dot-sym-name symtab)
                           (ast->code (slot-value a 'left) symtab)
                           (ast->code (slot-value a 'right) symtab)))

       ((ast-sym? a) (symtab-intern (slot-value a 'name) symtab))))))

(defun code-list? (c)
  "Tell if a code object contains a list."
  (listp (code-data c)))

(defun code-sym? (c)
  (sym? (code-data c)))
(defun code-sym-name (c)
  (sym-name (code-data c)))
(defun code-sym-id (c)
  (sym-id (code-data c)))
(defun code-car (c)
  (car (code-data c)))
(defun code-cadr (c)
  (cadr (code-data c)))
(defun code-cdr (c)
  (cdr (code-data c)))

(defun code-literal? (c)
  (with-slots (data) c
    (or (string? data) (num? data))))

(defun code-quoted-sym? (c)
  (with-slots (data) c
    (and (listp data)
         (length= data 2)
         (every #'code-sym? data)
         (string= quot-sym-name (code-sym-name (car data))))))

(defun code-op-is (c op-name)
  "Check whether C has a certain symbol in its operator position. Assumes C contains a list."
  (with-slots (data) c
    (and (code-sym? (car data))
         (string= (code-sym-name (car data)) op-name))))

(defun code->fnvalue (c)
  (with-slots (data) c
    (if (listp data)
        (apply #'fnlist (mapcar #'code->fnvalue data))
        data)))

(defun fnvalue->code (o x)
  (make-code o
             (if (fnlist? x)
                 (mapcar $(fnvalue->code o $) (fnlist->list x))
                 x)))

(defun walk-code (c &key list-fun quoted-list-fun atom-fun quoted-atom-fun)
  "
Walks fn code and applies the supplied functions in a way that respects quotation semantics.

Parameters
----------
  C                : code object to walk
  SYMTAB           : symbol table used by C
  LIST-FUN         : NIL or a function used to process to unquoted code lists (see below)
  QUOTED-LIST-FUN  : NIL or a function used to process to quoted code lists (see below)
  ATOM-FUN         : NIL or a function used to process to unquoted atoms (see below)
  QUOTED-ATOM-FUN  : NIL or a function used to process to quoted atoms (see below)

Returns
-------

  If C is an atom, returns the value of calling ATOM-FUN on C. If C is a list, returns the result of
  applying LIST-FUN.

Description
-----------

### Function parameters

ATOM-FUN and QUOTED-ATOM-FUN are called with one argument, the code object containing the atom. When
these are NIL, then the data slot of the atom is returned without processing.

LIST-FUN and QUOTED-LIST-FUN are called with two arguments. The first is the code object containing
the list _before_ recursive code walking, and the second is a descend function which accepts a list
as an argument. Calling the function will trigger recursive code walking on list. The reason for
this is that it allows the result to be modified both before and after recursive processing. This is
useful e.g. for macroexpansion, since the specific expansion of a macro will affect quoting
behavior.

For NIL values of LIST-FUN and QUOTED-LIST-FUN, recursive processing is done without any prior
transformations, and a list of the recursive results is returned (_not_ a code object).

Consequently, calling WALK-CODE with no keyword arguments will simply return a tree of fn atoms.

### Quoting semantics

The four quoting operators are `quote`, `quasiquote`, `unquote`, and `unquote-splice`. Each of these
accepts exactly one argument, and when the code walker finds a quoting operator with more than one
argument, it is processed as if it were normal code. (However, such code will raise an error during
syntax validation).

`quote` unconditionally quotes the provided expression, i.e. all recursive processing will only call
QUOTED-LIST-FUN and QUOTED-ATOM-FUN.

`unquote` and `unquote-splice` are only meaningful within quasiquoted forms, and outside of this
context they are processed as normal code. (In the syntax validation phase, having these operators
outside of quasiquote will generate an error.)

The argument to a quasiquote expression is quoted unless it is an 'unquote or 'unquote-splice
expression. The arguments to these expressions are unquoted. When quasiquote expressions are nested,
multiple levels of unquote forms are needed to trigger evaluation. For example, in a double
quasiquote expression, an unquote expression will be quoted, but an unquote expression _within_ an
unquote expression will be unquoted. unquote and unquote-splice operators that would result in
evaluation are stripped from the source tree, while others are preserved.
"
  (labels ((nil-atom-fun (c)
             (code-data c))
           (nil-list-fun (c descend)
             (funcall descend (code-data c))))
    (let ((af (or atom-fun #'nil-atom-fun))
          (qaf (or quoted-atom-fun #'nil-atom-fun))
          (lf (or list-fun #'nil-list-fun))
          (qlf (or quoted-list-fun #'nil-list-fun)))
      (labels
          ;; walker for unquoted expressions
          ((walk (c)
             (if (code-list? c)
                 (funcall lf
                          c
                          $(cond
                             ((null $) nil)
                             ((and (eq (code-data (car $)) quote-sym)
                                   (length= $ 2))
                              (list (code-data (car $))
                                    (walk-quoted (cadr $))))
                             ((and (eq (code-data (car $)) quasiquote-sym)
                                   (length= $ 2))
                              (list (code-data (car $))
                                    (walk-qquoted (cadr $) 1)))
                             (t (mapcar #'walk $))))
                 (funcall af c)))
           ;; for quoted expressions
           (walk-quoted (c)
             (if (code-list? c)
                 (funcall qlf
                          c
                          $(mapcar #'walk-quoted $))
                 (funcall qaf c)))
           ;; for quasiquoted expressions. LEVEL is the number of nested quasiquotes
           (walk-qquoted (c level)
             (if (code-list? c)
                 (funcall qlf
                          c
                          $(cond
                             ((null $) nil)
                             ((and (eq (code-data (car $)) quasiquote-sym)
                                   (length= $ 2))
                              (list (code-data (car $))
                                    (walk-qquoted (cadr $) (+ level 1))))
                             ((and (or (eq (code-data (car $)) unquote-sym)
                                       (eq (code-data (car $)) unquote-splice-sym))
                                   (length= $ 2))
                              (if (= level 1)
                                  (walk (cadr $))
                                  (list (code-data (car $))
                                        (walk-qquoted (cadr $) (- level 1)))))
                             (t (mapcar (lambda (x)
                                          (walk-qquoted x level))
                                        $))))
                 (funcall qaf c))))
        (walk c)))))

(defun validate-code-list (c descend)
  "Function to be used within WALK-CODE to verify the syntax of a code list."
  (let* ((d (code-data c))
         (op (if d (code-data (car d))))
         (args (cdr d))
         (o (code-origin c)))
    (cond
      ((null d) (fn-error (code-origin c) "empty list"))
      ;; special operators
      ((eq op apply-sym)
       (check-arg-length>= o args 2 "apply")
       (funcall descend d))
      ((eq op case-sym)
       (fn-error o "case is not implemented :("))
      ((eq op cond-sym)
       (check-arg-length>= o args 1 "cond")
       (unless (evenp (length args))
         (fn-error o "odd number of arguments to cond"))
       (funcall descend args))
      ((eq op def-sym) (validate-def o args descend))
      ((eq op defclass-sym) (validate-defclass o args descend))
      ((eq op defmacro-sym) (validate-defmacro o args descend))
      ((eq op defmethod-sym) (validate-defmethod o args descend))
      ((eq op defvar-sym) (validate-defvar o args descend))
      ((eq op do-sym)
       (check-arg-length>= o args 1 "do")
       (funcall descend args))
      ((eq op dollar-fn-sym)
       (check-arg-length= o args 1 "dollar-fn")
       (funcall descend args))
      ((eq op fn-sym)
       (check-arg-length>= o args 2 "fn")
       (validate-params o (code-data (car args)) "in fn parameters: ")
       (funcall descend (cdr args)))
      ((eq op get-sym))
      ((eq op get-field-sym))
      ((eq op let-sym)
       (check-arg-length>= o args 2 "let")
       (validate-let-bindings o (car args) descend)
       (funcall descend (cdr args)))
      ((eq op quote-sym)
       (check-arg-length= o args 1 "quote")
       (funcall descend d))
      ((eq op quasiquote-sym)
       (check-arg-length= o args 1 "quasiquote")
       (funcall descend d))
      ((eq op unquote-sym) (fn-error o "unquote outside of quasiquote"))
      ((eq op unquote-splice-sym) (fn-error o "unquote-splice outside of quasiquote"))
      ((eq op set-sym))
      (t (funcall descend d)))))

(defun validate-code (c)
  "Ensures that all special forms have correct syntax. Emits an fn-error on malformed expressions."
  (walk-code
   c
   :list-fun #'validate-code-list
   :quoted-list-fun
   (lambda (c descend) (funcall descend (code-data c)))))

(defun check-arg-length= (o args n name)
  (unless (length= args n)
    (fn-error o "wrong number of arguments for ~a" name)))
(defun check-arg-length>= (o args n name)
  (when (length< args n)
    (fn-error o "too few arguments for ~a" name)))

(defun validate-exprs (args)
  (mapc #'validate-code args))

(defun validate-params (o data &optional (prefix ""))
  (labels ((check-params (lst key opt)
             (if lst
                 (let ((p (car lst)))
                   (cond
                     ;; & starts variadic argument
                     ((and (code-sym? p)
                           (string= (code-sym-name p) "&"))
                      (check-vari-param lst))

                     ;; required positional argument
                     ((code-sym? p)
                      (when opt
                        (fn-error
                         o
                         "~a~a"
                         prefix
                         "required positionals parameters can't come after optional parameters"))
                      (when key
                        (fn-error
                         o
                         "~a~a"
                         prefix
                         "non-keyword parameters cannot follow keyword parameters"))
                      (check-params (cdr lst) nil nil))

                     ;; required keyword argument
                     ((code-quoted-sym? p)
                      (when opt
                        (fn-error
                         o
                         "~a~a"
                         prefix
                         "required keyword parameters cannot follow optional parameters"))
                      (check-params (cdr lst) t nil))

                     ;; optional arguments
                     ((and (code-list? p)
                           (length= (code-data p) 2))
                      (let ((name (code-car p)))
                        (if (code-quoted-sym? name)
                            (check-params (cdr lst) t opt)
                            (if key
                                (fn-error
                                 o
                                 "~a~a"
                                 prefix
                                 "non-keyword parameters cannot follow keyword parameters")
                                (check-params (cdr lst) nil t)))))

                     (t (fn-error o
                                  "~a~a"
                                  prefix
                                  "malformed parameter"))))))
           ;; check variadic parameter
           (check-vari-param (lst)
             (unless (length= lst 2)
               (fn-error o
                         "~a~a"
                         prefix
                         "too many parameters after &"))
             (unless (code-sym? (cadr lst))
               (fn-error o
                         "~a~a"
                         prefix
                         "malformed variadic parameter"))))
    (if (listp data)
        (check-params data nil nil)
        (fn-error o
                  "~a~a"
                  prefix
                  "parameters must form a list"))))

(defun validate-def (o args descend)
  (check-arg-length>= o args 2 "def")
  (let ((place (car args)))
    (cond
      ;; variable definition
      ((code-sym? place)
       (unless (length= args 2)
         (fn-error o "too many arguments in definition of ~a" (code-sym-name place))))
      ;; function definition
      ((and (code-list? place)
            (code-sym? (code-car place)))
       (validate-params o
                        (code-cdr place)
                        "in function definition parameters: "))
      ;; method definition
      ((and (code-list? place)
            (code-list? (code-car place))
            (code-sym? (code-car (code-car place))))
       (validate-params o
                        (code-cdr place)
                        "in method definition parameters: "))
      (t (fn-error o
                   "invalid place in def form")))
    (funcall descend (cdr args))))

(defun validate-defclass (o args descend)
  (declare (ignorable descend))
  (check-arg-length= o args 2 "defclass")
  (let ((name (car args))
        (params (cadr args)))
    (unless (code-sym? name)
      (fn-error o "invalid class name"))
    (validate-params o (code-data params) "in defclass parameters: ")))

(defun validate-defmacro (o args descend)
  (check-arg-length>= o args 2 "defmacro")
  (unless (and (code-list? (car args))
               (code-sym? (code-car (car args))))
    (fn-error o "malformed macro prototype"))
  (validate-params o (code-cdr (car args)) "in defmacro parameters: ")
  (funcall descend (cdr args)))

(defun validate-defmethod (o args descend)
  (declare (ignorable descend))
  (check-arg-length= o args 1 "defmethod")
  (let ((proto (car args)))
   (unless (and (code-list? proto)
                (code-list? (code-car proto))
                (code-sym? (code-car (code-car proto))))
     (fn-error o "malformed defmethod expression"))
   (validate-params o (code-cdr proto))))

(defun validate-defvar (o args descend)
  (check-arg-length= o args 2 "defvar")
  (let ((place (car args)))
    (unless (code-sym? place)
      (fn-error o "invalid place in defvar form")))
  (funcall descend (cdr args)))

(defun validate-let-bindings (o x descend)
  (unless (code-list? x)
    (fn-error o "let bindings must be a list"))
  (unless (evenp (length (code-data x)))
    (fn-error o "odd number of items in let binding list"))
  (let ((pairs (group 2 (code-data x))))
    (unless (every $(code-sym? (car $)) pairs)
      (fn-error o "let vars must be symbols"))
    (mapc $(validate-code (funcall descend (cdr $))) pairs)))
