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
(defconstant-1 true-sym-name "true")
(defconstant-1 false-sym-name "false")
(defconstant-1 null-sym-name "null")

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

     ((ast-paren? a)  (mapcar $(ast->code $ symtab)
                             (slot-value a 'contents)))
     ((ast-bracket? a) (cons (symtab-intern bracket-sym-name symtab)
                             (mapcar $(ast->code $ symtab)
                                     (slot-value a 'contents))))
     ((ast-brace? a) (cons (symtab-intern brace-sym-name symtab)
                           (mapcar $(ast->code $ symtab)
                                   (slot-value a 'contents))))

     ((ast-quot? a) (list (code-intern a quot-sym-name symtab)
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
(defun code-car (c)
  (car (code-data c)))
(defun code-cadr (c)
  (cadr (code-data c)))
(defun code-cdr (c)
  (cdr (code-data c)))

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

;;; runtime errors are handled by the eval function so that the offending line of code can be
;;; implicated

(eval-when (:load-toplevel :compile-toplevel :execute)
  (define-condition runtime-error (error)
    ((message :initarg :message)))

  (defmacro handling-runtime-errors (code-object &body body)
    "Converts runtime errors to code errors"
    `(handler-case (progn ,@body)
       (runtime-error (x)
         (code-error ,code-object (slot-value x 'message)))))

  (defmacro runtime-error (format-string &rest format-args)
    `(error 'runtime-error
            :message (format nil ,format-string ,@format-args))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; special operator creation macros

;;; macros for to make defining special/built-in operators and syntax checking more convenient

(eval-when (:load-toplevel :compile-toplevel :execute)

  (defparameter special-op-dispatch nil)

  (defparameter builtin-globals nil)

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
              (declare (ignorable c env interpreter))
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
                 (cdr specs))))))))

  (defmacro defun-builtin (name args-var &body body)
    "Define a builtin function and add it to the list"
    `(push (cons ,name
                 (make-fnfun :body (lambda (,args-var) ,@body)))
           builtin-globals))

  (defun-builtin "+" args
    (num (reduce #'+ args)))
  (defun-builtin "-" args
    (num (reduce #'- args)))
  (defun-builtin "*" args
    (num (reduce #'* args)))
  (defun-builtin "/" args
    (num (reduce #'/ args)))
  (defun-builtin "print" args
    (if (length= args 1)
        (fnprint (car args))
        (runtime-error "print called with too many arguments")))
  (defun-builtin "println" args
    (if (length= args 1)
        (progn (fnprintln (car args))
               (terpri))
        (runtime-error "println called with too many arguments")))
  (defun-builtin "String" args
    (apply #'fnstring args))
  (defun-builtin "List" args
    (apply #'fnlist args))
  (defun-builtin "exit" args
    (funcall #'sb-ext:exit
             :code (if args (floor (car args)) 0)))

  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; main interpreter code

(defstruct interpreter
  (globals (init-env nil))
  (macros (init-env nil))
  (symtab (make-instance 'symtab)))

(defun set-global (interpreter sym cell)
  (setf (gethash (sym-id sym)
                 (slot-value interpreter 'globals))
        cell))

(defun init-interpreter ()
  "Initialize an interpreter with all defined built-in globals"
  (let ((res (make-interpreter)))
    (loop for x in builtin-globals
       do (set-global res
                      (fnintern (car x) res)
                      (make-cell :value (cdr x))))
    res))

(defun get-macro (sym interpreter)
  (gethash (sym-id sym) (slot-value interpreter 'macros)))

(defun fnintern (name interpreter)
  "Get an internal symbol using INTERPRETER's symbol table."
  (symtab-intern name (interpreter-symtab interpreter)))

(defparameter *global-interpreter* (init-interpreter))

(defun eval-code (c &optional (env (init-env nil)) (interpreter *global-interpreter*))
  "Evaluate a code object."
  (handling-runtime-errors c
    (cond
      ((code-literal? c) (code-data c))
      ((code-sym? c)
       (cond
         ((string= (code-sym-name c) true-sym-name) true)
         ((string= (code-sym-name c) false-sym-name) false)
         ((string= (code-sym-name c) null-sym-name) fnnull)
         (t (eval-var c env interpreter))))
      ((code-list? c) (eval-command c env interpreter))
      (t (fn-error "FN.EVAL"
                   "invalid code object passed to EVAL-CODE")))))

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
    (if (code-sym? op)
        (let ((sym (code-data op)))
          (aif (assoc (sym-name sym)
                      special-op-dispatch
                      :test #'equal)
               (funcall (cdr it) c env interpreter)
               (aif (get-macro sym interpreter)
                    (code-error c "Macros aren't implemented")
                    (eval-funcall c env interpreter))))
        (eval-funcall c env interpreter))))

(defun eval-body (body env interpreter)
  "Evaluate a list of code objects in order, returning the last expresison."
  (cond
    ((null body) (runtime-error "No expressions in body"))
    ((length= body 1) (eval-code (car body) env interpreter))
    (t (eval-code (car body) env interpreter)
       (eval-body (cdr body) env interpreter))))

(defun bind-params (param-list args outer-env interpreter)
  "Extend OUTER-ENV with variable bindings created by PARAM-LIST and ARGS."
  (with-slots (pos keyword vari) param-list
    (labels ((get-optional (p)
               (if (cdr p)
                   (eval-code (cdr p) outer-env interpreter)
                   (runtime-error "Missing required argument for parameter list")))
             (bind-positional (acc params args)
               (cond
                 ((null params)
                  (if keyword
                      (append acc (bind-keyword nil nil args))
                      (append (bind-vari args) acc)))
                 ((null args)
                  (bind-positional (cons (cons (caar params)
                                               (get-optional (car params)))
                                         acc)
                                   (cdr params)
                                   nil))
                 (t (bind-positional (cons (cons (caar params)
                                                 (car args))
                                           acc)
                                     (cdr params)
                                     (cdr args)))))
             ;; vari-acc tracks variadic keyword args where applicable
             (bind-keyword (acc vari-acc args)
               (if args
                   (let* ((k (car args))
                          (v (cadr args)))
                     (unless (sym? k)
                       (runtime-error "Keyword is not a symbol"))
                     (when (null v)
                       (runtime-error "Odd number of keyword arguments"))
                     (if (assoc k keyword :test #'equal)
                         (bind-keyword (cons (cons k v) acc)
                                       vari-acc
                                       (cddr args))
                         (if vari
                             (bind-keyword acc
                                           (cons (cons k v) vari-acc)
                                           (cddr args))
                             (runtime-error "Unknown keyword ~s"
                                            (sym-name k)))))
                   ;; make sure all required keywords are bound
                   (let ((res
                          (mapcar $(or (assoc (car $) acc :test #'equal)
                                       (get-optional $))
                                  keyword)))
                     (if vari
                         (cons (cons vari (apply #'fnlist vari-acc))
                               res)
                         res))))
             (bind-vari (args)
               (if vari
                   (list (cons vari args))
                   (if args
                       (runtime-error "Too many arguments")
                       nil))))
      (let* ((binding-alist (bind-positional nil pos args))
             (res (init-env (mapcar #'car binding-alist) outer-env)))
        (mapc $(set-var (car $) (cdr $) res interpreter) binding-alist)
        res))))

(defun eval-funcall (c env interpreter)
  "Evaluate a function call."
  (let ((args (mapcar $(eval-code $ env interpreter)
                      (code-cdr c)))
        (op (eval-code (code-car c) env interpreter)))
    (unless (fnfun? op)
      (runtime-error "Operator is not a function"))
    (with-slots (params body closure) op
      (if (functionp body)
          (funcall body args)
          (let ((env (bind-params params args closure interpreter)))
            (eval-body body env interpreter))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; special operators

(defun fn-quoted? (c)
  (and (code-list? c)
       (code-sym? (car (code-data c)))
       (string= (code-sym-name (car (code-data c)))
                quot-sym-name)))

(defun validate-def-name (c)
  (unless (or (code-sym? c)
              (and (code-list? c)
                   (code-sym? (car (code-data c)))))
    (code-error c "Illegal name in definition")))

(defun validate-def-value (lst)
  (unless lst
    (runtime-error "Missing value in definition")))

;; TODO: right now, new definitions clobber old ones. This behavior is useful for testing, but not
;; dangerous in practice, so it should at least emit a warning.
(def-op "def" ((name #'validate-def-name) & (value-or-body #'validate-def-value))
  (if (code-list? name)
      (eval-function-def (code-data name)
                         value-or-body
                         env
                         interpreter)
      (if (length= value-or-body 1)
          (let ((v (eval-code (car value-or-body))))
            (env-add (slot-value interpreter 'globals)
                     (code-data name)
                     (make-cell :value v :mutable nil))
            v)
          (code-error c "Too many arguments in def expression"))))


(def-op "defvar" ((name #'validate-def-name) & (value-or-body #'validate-def-value))
  (if (code-list? name)
      (eval-function-def (code-data name)
                         value-or-body
                         env
                         interpreter)
      (if (length= value-or-body 1)
          (let ((v (eval-code (car value-or-body))))
            (env-add (slot-value interpreter 'globals)
                     (code-data name)
                     (make-cell :value v :mutable t))
            v)
          (code-error c "Too many arguments in defvar expression"))))

(defun eval-function-def (name body env interpreter)
  (let ((fun (make-fnfun :params (code->param-list (cdr name))
                         :body body
                         :closure env)))
   (env-add (slot-value interpreter 'globals)
            (code-data (car name))
            (make-cell :value fun))
   fun))

(defun validate-set-place (c)
  (unless (code-sym? c)
    (code-error c "Illegal place in set expression")))

(defun set-var (name value env interpreter)
  (let ((cell (find-cell name env interpreter)))
    (cond
      ((null cell)
       (runtime-error "Undefined variable ~s" (sym-name name)))

      ((not (cell-mutable cell))
       (runtime-error "Attempt to set immutable variable ~s"
                      (sym-name name)))

      (t (cell-mutable cell)
         (setf (cell-value cell) value)))))

(def-op "set" ((place #'validate-set-place) value)
  (cond
    ((code-sym? place) (set-var (code-data place)
                                (eval-code value env interpreter)
                                env
                                interpreter))
    (t (code-error c "Unrecognized set place"))))

(defun code-quoted-sym? (c)
  (with-slots (data) c
    (and (listp data)
         (length= data 2)
         (every #'code-sym? data)
         (string= quot-sym-name (code-sym-name (car data))))))

;; TODO: check for duplicate param names
(defun validate-params (c)
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
                        (code-error
                         c
                         "Required positionals parameters can't come after optional parameters"))
                      (when key
                        (code-error
                         c
                         "Non-keyword parameters cannot follow keyword parameters"))
                      (check-params (cdr lst) nil nil))

                     ;; required keyword argument
                     ((code-quoted-sym? p)
                      (when opt
                        (code-error
                         c
                         "Required keyword parameters cannot follow optional parameters"))
                      (check-params (cdr lst) t nil))

                     ;; optional arguments
                     ((and (code-list? p)
                           (length= (code-data p) 2))
                      (let ((name (code-car p)))
                        (if (code-quoted-sym? name)
                            (check-params (cdr lst) t t)
                            (if key
                                (code-error
                                 c
                                 "Non-keyword parameters cannot follow keyword parameters")
                                (check-params (cdr lst) nil t)))))

                     (t (code-error c "Malformed parameter"))))))
           ;; check variadic parameter
           (check-vari-param (lst)
             (unless (length= lst 2)
               (code-error c "Too many parameters after &"))
             (unless (code-sym? (cadr lst))
               (code-error c "Malformed variadic parameter"))))
    (and (code-list? c)
         (check-params (code-data c) nil nil))))

(defun code->param-list (lst)
  "Takes a list of code objects and generates a param-list"
  (let* ((non-vari
          (take-while $(not (and (code-sym? $)
                                 (string= (code-sym-name $) "&")))
                      lst))
         (pos (remove-if-not $(or (and (code-sym? $)
                                       (not (string= (code-sym-name $) "&")))
                                  (and (code-list? $)
                                       (code-sym? (code-car $))))
                             non-vari))
         (keyword (remove-if-not $(or (code-quoted-sym? $)
                                      (and (code-list? $)
                                           (code-quoted-sym? (code-car $))))
                                 non-vari))
         (vari (if (some $(and (code-sym? $)
                               (string= (code-sym-name $) "&"))
                         lst)
                   (code-data (car (last lst)))
                   nil)))
    (make-param-list :pos (mapcar $(if (code-sym? $)
                                       (cons (code-data $) nil)
                                       (cons (code-data (code-car $))
                                             (code-cadr $)))
                                  pos)
                     :keyword (mapcar $(if (code-quoted-sym? $)
                                           (cons (code-data (code-cadr $)) nil)
                                           (cons (code-data (code-cadr (code-car $)))
                                                 (code-cadr $)))
                                      keyword)
                     :vari vari)))

(def-op "fn" ((params #'validate-params) & body)
  (make-fnfun :params (code->param-list (code-data params))
              :body body
              :closure env))
