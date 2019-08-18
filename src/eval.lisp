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
  (:use :cl :fn.util :fn.ast :fn.values :fn.code)
  (:export :eval-ast :eval-code))

(in-package :fn.eval)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; runtime errors are handled by the eval function so that the offending line of code can be
;;; implicated

(eval-when (:load-toplevel :compile-toplevel :execute)
  (define-condition runtime-error (error)
    ((message :initarg :message)))

  (defmacro handling-runtime-errors (code &body body)
    "Converts runtime errors to fn errors"
    (with-gensyms (x code-sym)
      `(let ((,code-sym ,code))
         (handler-case (progn ,@body)
           (runtime-error (,x)
             (fn-error (code-origin ,code-sym)
                       "Runtime error: ~a"
                       (slot-value ,x 'message)))))))

  (defmacro runtime-error (format-string &rest format-args)
    `(error 'runtime-error
            :message (format nil ,format-string ,@format-args)))

  ;; one day this will be implemented
  (defmacro runtime-warning (format-string &rest format-args)
    (declare (ignore format-string format-args))
    nil))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; special operator creation macros

;;; macros for to make defining special/built-in operators and syntax checking more convenient

(eval-when (:load-toplevel :compile-toplevel :execute)

  (defparameter special-op-dispatch nil)

  (defparameter builtin-globals nil)

  (defmacro def-op (name params &body body)
    "DEF-OP defines a special operator in fn by adding it to SPECIAL-OP-DISPATCH.

 NAME       : a string denoting the operator's name
 PARAMS     : common lisp lambda list describing arguments
 BODY       : function body describing the operator's behavior

 All the names in the ARG-SPECs list are bound to corresponding arguments for use in the function
 body. In addition, variables named C, ENV, and INTERPRETER are bound holding evaluation
 parameters (C is the code object for the current expression)."
    `(push
      (cons ,name
            (lambda (c env interpreter)
              (declare (ignorable c env interpreter))
              (destructuring-bind ,params (code-cdr c)
                ,@body)))
      special-op-dispatch))

  (defmacro def-builtin (name value)
    "Define a built-in variable"
    `(push (cons ,name ,value) builtin-globals))

  (defmacro defun-builtin (name args-var &body body)
    "Define a builtin function and add it to the list"
    `(push (cons ,name
                 (make-fnfun :body (lambda (,args-var) ,@body)))
           builtin-globals))

  (defun error-constructor (args)
    (declare (ignore args))
    (runtime-error "Can't construct this type"))
  (defmacro def-builtin-class (sym &key (fields 'nil) (constructor '#'error-constructor))
    (with-gensyms (sym-var)
      `(let ((,sym-var ,sym)) 
         (def-builtin (sym-name ,sym-var)
             (make-fnclass :name ,sym-var
                           :fields ,fields
                           :constructor (make-fnfun :body ,constructor))))))

  ;; classes
  (def-builtin-class bool-class-sym)
  (def-builtin-class class-class-sym
      :fields (list name-sym fields-sym constructor-sym))
  (def-builtin-class function-class-sym)
  (def-builtin-class list-class-sym
      :constructor $(apply #'fnlist $))
  (def-builtin-class module-class-sym
      :fields (list name-sym vars-sym macros-sym))
  (def-builtin-class method-class-sym)
  (def-builtin-class null-class-sym)
  (def-builtin-class num-class-sym)
  (def-builtin-class string-class-sym
      :constructor $(apply #'fnstring $))

  (defun-builtin "+" args
    (num (reduce #'+ args)))
  (defun-builtin "-" args
    (num (reduce #'- args)))
  (defun-builtin "*" args
    (num (reduce #'* args)))
  (defun-builtin "/" args
    (num (reduce #'/ args)))
  (defun-builtin "=" args
    (if (apply #'equal args)
        true
        false))
  (defun-builtin "print" args
    (if (length= args 1)
        (fnprint (car args))
        (runtime-error "print called with too many arguments"))
    fnnull)
  (defun-builtin "println" args
    (if (length= args 1)
        (fnprintln (car args))
        (runtime-error "println called with too many arguments"))
    fnnull)
  (defun-builtin "exit" args
    (funcall #'sb-ext:exit
             :code (if args (floor (car args)) 0)))

  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; interpreter state and name resolution

(defstruct interpreter
  (modules)
  (symtab (make-symtab)))

;; defined before initialization to avoid compiler warning
(defparameter *current-env* nil "Lexical environment used for evaluation")
(defparameter *interpreter* nil "Interpreter used for evaluation")

(defun init-interpreter ()
  "Initialize an interpreter with built-in globals and symbols and a default module."
  (let* ((symtab (make-symtab))
         (mod (make-fnmodule :name (symtab-intern "__default" symtab)))
         (res (make-interpreter :modules (list mod) :symtab symtab)))
    (loop for x in builtin-globals
       do (add-module-cell mod
                           (fnintern (car x) res)
                           (make-cell :value (cdr x))))
    res))

(defun init-env (syms &optional (parent *current-env*) (module (current-module)))
  "Create an ENV with uninitialized value cells for each symbol in SYMS."
  (let ((table (make-hash-table :size (min (ceiling (* 1.5 (length syms)))
                                           10))))
    (map nil
         $(setf (gethash (sym-id $) table)
                (make-cell :mutable t))
         syms)
    (make-env :table table
              :module module
              :parent parent)))

(defun fnintern (name &optional (interpreter *interpreter*))
  "Get an internal symbol using INTERPRETER's symbol table."
  (symtab-intern name (interpreter-symtab interpreter)))

(setf *interpreter* (init-interpreter))
(setf *current-env* (init-env nil nil (car (interpreter-modules *interpreter*))))

(defun current-module ()
  (env-module *current-env*))


(defun search-for-module (str)
  (runtime-error "Could not find module ~a" str))

(defun find-module (sym)
  (aif (find-if $(eq sym (fnmodule-name $))
                (interpreter-modules *interpreter*))
       (cdr it)
       (search-for-module (sym-name sym))))


(defun safe-add-global (sym value &optional mutable function-name)
  "Add a global variable without trampling any definitions."
  (aif (get-module-cell (current-module) sym)
       (if (cell-mutable it)
           (progn
             (runtime-warning "~a: Redefining variable ~a"
                              function-name
                              (->string sym))
             (setf (cell-value it) value))
           (runtime-error "~a: Attempt to redefine immutable variable ~a"
                          function-name
                          (->string sym)))
       (add-global-cell *current-env* sym (make-cell :value value
                                                     :mutable mutable))))

(defun get-var (name)
  (aif (get-cell *current-env* name)
       (cell-value it)
       (runtime-error "Undefined variable ~s" (sym-name name))))

(defun set-var (name value)
  (let ((cell (get-cell *current-env* name)))
    (cond
      ((null cell)
       (runtime-error "Undefined variable ~s" (sym-name name)))

      ((not (cell-mutable cell))
       (runtime-error "Attempt to set immutable variable ~s"
                      (sym-name name)))

      (t (cell-mutable cell)
         (setf (cell-value cell) value)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; main evaluation functions

(defun eval-ast (a &optional (env *current-env*) (interpreter *interpreter*))
  "Convert an AST object to code, check it syntax errors, and evaluate it."
  (let ((code (ast->code a (interpreter-symtab interpreter))))
    (validate-code code)
    (eval-code code env interpreter)))

(defun eval-code (c &optional (env *current-env*) (interpreter *interpreter*))
  "Evaluate a code object."
  (let ((*current-env* env)
        (*interpreter* interpreter))
    (handling-runtime-errors c
      (cond
        ((code-literal? c) (code-data c))
        ((code-sym? c)
         (cond
           ((string= (code-sym-name c) true-sym-name) true)
           ((string= (code-sym-name c) false-sym-name) false)
           ((string= (code-sym-name c) null-sym-name) fnnull)
           (t (eval-var c))))
        ((code-list? c) (eval-list c))
        (t (fn-error (code-origin c)
                     "invalid code object passed to EVAL-CODE"))))))

(defun eval-var (c)
  "Evaluate a variable reference. Assumes CODE contains a symbol"
  (aif (get-cell *current-env* (code-data c))
       (cell-value it)
       (runtime-error "Unbound symbol: ~a" (code-sym-name c))))

(defun eval-body (body)
  "Evaluate a list of code objects in order, returning the last expresison."
  (cond
    ((null body) (runtime-error "No expressions in body"))
    ((length= body 1) (eval-code (car body)))
    (t (eval-code (car body))
       (eval-body (cdr body)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; List evaluation

(defun eval-list (c)
  (let* ((op (code-data (code-car c)))
         (args (code-cdr c)))
    (cond
      ((eq op apply-sym) (eval-apply args))
      ((eq op class-of-sym) (eval-class-of (car args)))
      ((eq op cond-sym) (eval-cond args))
      ((eq op def-sym) (eval-def (car args) (cdr args)))
      ((eq op defclass-sym)
       (eval-defclass (code-data (code-car (car args)))
                      (code-cdr (car args))))
      ((eq op defmacro-sym)
       (eval-defmacro (code-data (code-car (car args)))
                      (code-cdr (car args))
                      (cdr args)))
      ((eq op defmethod-sym)
       (eval-defmethod (code-data (code-caar (car args)))
                       (code-cdar (car args))
                       (code-cdr (car args))))
      ((eq op defvar-sym)
       (eval-defvar (code-data (car args))
                    (cadr args)))
      ((eq op do-sym)
       (eval-do args))
      ((eq op dollar-fn-sym)
       (eval-dollar-fn (car args)))
      ((eq op fn-sym)
       (eval-fn (car args) (cdr args)))
      ((eq op get-sym)
       (eval-get (car args) (cdr args)))
      ((eq op get-field-sym)
       (eval-get-field (car args) (cdr args)))
      ((eq op if-sym)
       (eval-if (car args) (cadr args) (caddr args)))
      ((eq op quasiquote-sym)
       (eval-quasiquote (cadr args)))
      ((eq op quote-sym)
       (eval-quote (cadr args)))
      ((eq op set-sym)
       (eval-set (car args) (cadr args)))
      ((eq op unquote-sym)
       (runtime-error "unquote outside of quasiquote"))
      ((eq op unquote-splice-sym)
       (runtime-error "unquote-splice outside of quasiquote"))
      ((get-expr? op) (eval-get-op-list c op args))
      ((not (sym? op)) (eval-funcall c))
      (t
       (aif (assoc (sym-name op)
                     special-op-dispatch
                     :test #'equal)
            (funcall (cdr it) c *current-env* *interpreter*)            
            (aif (get-macro *current-env* op)
                 (let ((code (expand-macro it c)))
                   (validate-code code)
                   (eval-code code))
                 (eval-funcall c)))))))

(defun get-expr? (c)
  "Tell if C is a get expression"
  ;; assume the validator has already checked the number of arguments
  (and (code-list? c)
       (eq (code-car c) get-sym)))

(defun eval-get-op-list (c op args-code)
  "Evaluate a list where the CAR is a get operation."
  (if (code-quoted-sym? (code-caddr op))
      (let ((key (eval-code (code-caddr op)))
            (get-obj (eval-code (code-cadr op))))
        (if (fnmodule? get-obj)
            (aif (get-module-macro get-obj key)
                 (let ((code (expand-macro it c)))
                   (validate-code code)
                   (eval-code code))
                 (call-obj (get-key get-obj key)
                           (mapcar #'eval-code args-code)))))
      (call-obj (eval-code op) (mapcar #'eval-code args-code))))

(defun bind-params (param-list args outer-env)
  "Extend OUTER-ENV with variable bindings created by PARAM-LIST and ARGS."
  (with-slots (pos keyword vari) param-list
    (labels ((get-optional (p)
               (if (cdr p)
                   (eval-code (cdr p) outer-env)
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
                             ;; vari-acc is built backwards
                             (bind-keyword acc
                                           (cons v (cons k vari-acc))
                                           (cddr args))
                             (runtime-error "Unknown keyword ~s"
                                            (sym-name k)))))
                   ;; make sure all required keywords are bound
                   (let ((res
                          (mapcar $(or (assoc (car $) acc :test #'equal)
                                       (cons (car $) (get-optional $)))
                                  keyword)))
                     (if vari
                         (cons (cons vari (apply #'fnlist (nreverse vari-acc)))
                               res)
                         res))))
             (bind-vari (args)
               (if vari
                   (list (cons vari (apply #'fnlist args)))
                   (if args
                       (runtime-error "Too many arguments")
                       nil))))
      (let* ((binding-alist (bind-positional nil pos args))
             (*current-env* (init-env (mapcar #'car binding-alist) outer-env)))
        (mapc $(set-var (car $) (cdr $)) binding-alist)
        *current-env*))))

(defun expand-macro (macro-fun c)
  (with-slots (filename line column) (code-origin c)
    ;; TODO: change so that the origin looks better with dot syntax
    (let* ((origin (make-origin :filename filename
                                :line line
                                :column column
                                :macro (->string (code-data (code-car c)))))
           (args (mapcar #'code->fnvalue (code-cdr c))))
      (fnvalue->code (call-fun macro-fun args) origin))))

(defun call-fun (fnfun args)
  (with-slots (params body closure) fnfun
    (if (functionp body)
        (funcall body args)
        (let ((*current-env* (bind-params params args closure)))
          (eval-body body)))))

(defun call-fnmethod (m args)
  (with-slots (params dispatch-params default-impl) m
    (let* ((new-env (bind-params params args *current-env*))
           (types (mapcar $(get-class-of (cell-value (get-cell new-env $)))
                          dispatch-params)))
      (aif (get-impl m types)
           (call-fun it args)
           (if default-impl
               (call-fun default-impl args)
               (runtime-error "Method not implemented on types (~{~a~^ ~})."
                              (mapcar $(->string (fnclass-name $)) types)))))))

(defun call-obj (op args)
  (cond
    ((fnfun? op) (call-fun op args))
    ((fnmethod? op) (call-fnmethod op args))
    ((fnclass? op) (call-fun (fnclass-constructor op) args))
    (t (runtime-error "Operator is not a callable object"))))

(defun eval-funcall (c)
  "Evaluate a function call."
  (let ((op (eval-code (code-car c)))
        (args (mapcar #'eval-code
                      (code-cdr c))))
    (call-obj op args)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; special operators

;;; apply
(defun eval-apply (arg-exprs)
  (let ((op (eval-code (car arg-exprs)))
        (args (mapcar $(eval-code $) (cdr arg-exprs))))
    (unless (fnlist? (car (last args)))
      (runtime-error "apply: last argument must be a list"))
    (let ((arg-list (reduce #'cons args :from-end t)))
      (rplacd (last arg-list) nil)
      (call-obj op arg-list))))

;;; class-of
(defun eval-class-of (arg-code)
  (or (get-class-of (eval-code arg-code))
      (runtime-error "object has unknown class")))

(defun get-class-of (v)
  (cond ((or (true? v) (false? v)) (class-by-name (fnintern "Bool")))
        ((fnclass? v) (class-by-name (fnintern "Class")))
        ((fnfun? v) (class-by-name (fnintern "Function")))
        ((fnlist? v) (class-by-name (fnintern "List")))
        ((fnmodule? v) (class-by-name (fnintern "Module")))
        ((fnmethod? v) (class-by-name (fnintern "Method")))
        ((fnnull? v) (class-by-name (fnintern "Null")))
        ((num? v) (class-by-name (fnintern "Num")))
        ((string? v) (class-by-name (fnintern "String")))
        ((fnobj? v) (fnobj-class v))))

(defun class-by-name (sym)
  (aif (get-module-cell (current-module) sym)
       (if (fnclass? (cell-value it))
           (cell-value it)
           (runtime-error "not a class: ~a" (->string sym)))
       (runtime-error "no such class: ~a" (->string sym))))

;;; cond
(defun eval-cond (body)
  (let ((clauses (group 2 body)))
    (rlambda (src) (clauses)
      (when (null src)
        (runtime-error "every cond clause failed"))
      (let ((x (eval-code (caar src))))
        (if (or (eq x fnnull)
                (eq x false))
            (recur (cdr clauses))
            (eval-code (cadar src)))))))

;;; def
(defun eval-def (name-or-proto value-or-body)
  (if (code-list? name-or-proto)
      (let ((name (code-car name-or-proto))
            (params (code-cdr name-or-proto)))
        (if (code-list? name)
            (eval-method-def (code-data (code-car name))
                             (mapcar #'code-data (code-cdr name))
                             params
                             value-or-body)
            (eval-function-def (code-data name) params value-or-body)))
      (let ((v (eval-code (car value-or-body))))
        (safe-add-global (code-data name-or-proto) v nil "def")
        v)))

(defun eval-function-def (name params body)
  (let ((fun (make-fnfun :params (code->param-list params)
                         :body body
                         :closure *current-env*)))
    (safe-add-global name fun nil "def")
    fun))

(defun eval-method-def (name class-syms params body)
  (let* ((cell (get-var name))
         (m (if cell (cell-value cell))))
    (unless m
      (runtime-error "def: No such method ~s." (->string name)))
    (unless (fnmethod? m)
      (runtime-error "def: ~s is not a method." (->string name)))
    (unless (eql (length class-syms) (length (fnmethod-dispatch-params m)))
      (runtime-error "def: Incorrect number of dispatch-params. dispatch-params for ~s are \"(~{~a~^ ~})\"."
                     (->string name)
                     (fnmethod-dispatch-params m)))
    (unless (param-list-eqv (fnmethod-params m) params)
      (runtime-error
       "def: Mismatched method parameter list. ~s parameters are \"(~{~a~^ ~})\"."
       (->string name)
       (mapcar #'->string (fnmethod-params m))))
    (let ((types (mapcar $(class-by-name $) class-syms)))
      (if (get-impl m types)
          (runtime-error "def: Method ~a already defined on types \"(~{~a~^ ~})\"."
                         (->string name)
                         (mapcar #'->string class-syms))
          (set-impl m types (make-fnfun :params (code->param-list params)
                                        :body body
                                        :closure *current-env*))))))

;;; defclass
(defun eval-defclass (name params-code)
  (let* ((params (code->param-list params-code))
         (fields (param-list-vars params))
         (new-class (make-fnclass :name name :fields fields)))
    (setf (fnclass-constructor new-class) (make-constructor new-class params))
    (safe-add-global name new-class  nil "defclass")
    new-class))

(defun make-constructor (class params)
  "Create a constructor for the provided class"
  (with-slots (name fields) class
    (make-fnfun :params params
                :body $(make-fnobj :class class
                                   :contents (env-table (bind-params params $ nil))))))

;;; defmacro
(defun eval-defmacro (name params-code body-code)
  (set-global-macro *current-env*
                    name
                    (make-fnfun :params (code->param-list params-code)
                                :body body-code
                                :closure *current-env*))
  fnnull)

;;; defmethod
(defun eval-defmethod (name dispatch-params params)
  "Create a new method."
  (let ((m (make-fnmethod :params (code->param-list params)
                          :dispatch-params (mapcar #'code-data dispatch-params))))
    (safe-add-global name m nil "defmethod")))

;;; defvar
(defun eval-defvar (name value)
  (let ((v (eval-code value)))
    (safe-add-global name v t "defvar")
    v))

;;; do
(defun eval-do (body)
  (eval-body body))

;;; dollar-fn
(defun eval-dollar-fn (expr)
  (let* (($-syms (find-$-syms expr))
         (max-id (reduce $(if (> $0 $1) $0 $1)
                         (mapcar #'$-sym-id $-syms)
                         :initial-value -1))
         ;; check for variadic args
         (has-vari (member "$&"
                           $-syms
                           :test #'equal
                           :key #'sym-name))
         (pos (loop for i from 0 to max-id
                 ;; remember the CONS b/c parameters may have optional values
                 collect (cons (fnintern (format nil "$~a" i))
                               nil)))
         (params (make-param-list :pos pos
                                  :vari (and has-vari
                                             (fnintern "$&"))))
         (closure *current-env*))
    (make-fnfun :body
                (lambda (args)
                  (let ((env0 (bind-params params args closure)))
                    ;; add the alternative symbol for the first arg
                    (if (>= max-id 0)
                        (add-cell env0
                                  (fnintern "$")
                                  (get-cell env0 (fnintern "$0"))))
                    (eval-code expr env0))))))

(defun dollar-id-string? (str)
  ;; we have to ensure there are no superfluous leading 0's b/c then there wouldn't be one unique
  ;; dollar symbol per natural number
  (or (string= str "&")
      (string= str "0")
      (and (member (aref str 0)
                   '(#\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))
           (every #'digit-char-p str))))

(defun is-$-sym? (c)
  (if (code-sym? c)
      (let ((name (code-sym-name c)))
        (and (eql (aref name 0) #\$)
             (or (string= (subseq name 1) "")
                 (dollar-id-string? (subseq name 1)))))
      nil))

(defun find-$-qquote (expr level)
  "Descend into a quasiquoted expression, finding all unquoted $-syms."
  (cond
    ((not (code-list? expr))
     nil)
    ((null (code-data expr)) nil)
    ((code-sym? (code-car expr))
     (let ((op (code-data (code-car expr))))
       (cond
         ((and (eq op quasiquote-sym)
               (length= (code-data expr) 2))
          (find-$-qquote (code-cadr expr) (+ level 1)))
         ((and (or (eq op unquote-sym)
                   (eq op unquote-splice-sym))
               (length= (code-data expr) 2))
          (if (= level 1)
              (find-$-syms (code-cadr expr))
              (find-$-qquote (code-cadr expr) (- level 1))))
         (t (mapcan $(find-$-qquote $ level) (code-data expr))))))
    (t (mapcan $(find-$-qquote $ level) (code-data expr)))))

(defun find-$-syms (expr)
  (cond
    ((not (code-list? expr))
     (if (is-$-sym? expr)
         (list (code-data expr))
         nil))

    ((null (code-data expr)) nil)

    ((code-sym? (code-car expr))
     (let ((op (code-data (code-car expr))))
       (cond
         ((eq op quasiquote-sym)
          (find-$-qquote (code-cadr expr) 1))
         ((eq op quote-sym) nil)
         ((eq op dollar-fn-sym) nil)
         (t (mapcan #'find-$-syms (code-data expr))))))

    (t (mapcan #'find-$-syms (code-data expr)))))

(defun $-sym-id (sym)
  (let ((n-str (subseq (sym-name sym) 1)))
    (cond
      ((string= n-str "") 0)
      ((string= n-str "&") -1)
      (t (reduce $(+ (* 10 $0)
                     (- (char-code $1)
                        (char-code #\0)))
                 n-str
                 :initial-value 0)))))

;;; fn
(defun eval-fn (params body)
  (make-fnfun :params (code->param-list (code-data params))
              :body body
              :closure *current-env*))

;;; get and get-field
(defun eval-get (obj-code keys-code)
  (let ((obj (eval-code obj-code))
        (keys (mapcar #'eval-code keys-code)))
    (reduce #'get-key keys :initial-value obj)))

(defun get-key (obj key)
  (cond
    ((fnlist? obj) (fnlist-get obj key))
    ((fnclass? obj) (fnclass-get-field obj key))
    ((fnmodule? obj) (fnmodule-get obj key))
    ((string? obj) (fnstring-get obj key))
    ;; TODO: add check for get-method implementations
    ((fnobj? obj) (fnobj-get-field obj key))
    (t (runtime-error "Object ~s has no gettable fields or indices" (->string obj)))))

(defun fnmodule-get (obj key)
  "Gets a variable from a module by name"
  (unless (sym? key)
    (runtime-error "get: Key for module ~s is not a symbol: ~s"
                   (->string (fnmodule-name obj))
                   (->string key)))
  (aif (get-module-cell obj key)
       (cell-value it)
       (runtime-error "get: Module ~s contains no variable named ~s"
                      (->string (fnmodule-name obj))
                      (->string key))))

;; TODO: add support for negative indices
(defun fnlist-get (obj key)
  (cond
    ((num? key)
     (unless (num-int? key)
       (runtime-error "list index is not an integer: ~a" (->string key)))
     (unless (>= key 0)
       (runtime-error "list index is negative: ~a" (->string key)))
     (rlambda (pos lst) ((truncate key) obj)
       (if (empty? lst)
           (runtime-error "list index out of bounds: ~a" (->string key))
           (if (zerop pos)
               (car lst)
               (recur (- pos 1) (cdr lst))))))
    ((eq key hd-sym)
     (if (empty? obj)
         (runtime-error "empty list has no hd")
         (car obj)))
    ((eq key tl-sym)
     (if (empty? obj)
         (runtime-error "empty list has no tl")
         (cdr obj)))
    (t (fn-error "list has no field named ~a" (->string key)))))

(defun fnstring-get (obj key)
  (unless (num? key)
    (fn-error "string index is not a number: ~a" (->string key)))
  (unless (num-int? key)
    (fn-error "string index is not an integer: ~a" (->string key)))
  (unless (>= 0 key)
    (fn-error "string index is negative: ~a" (->string key)))
  (if (> key (length obj))
      (fn-error "string index out of bounds: ~a" (->string key))
      (aref obj (truncate key))))

(defun eval-get-field (obj-code fields-code)
  (let ((obj (eval-code obj-code))
        (fields (mapcar #'eval-code fields-code)))
    (reduce #'get-field fields :initial-value obj)))

(defun get-field (obj field)
  (cond
    ((fnclass? obj) (fnclass-get-field obj field))
    ((fnmodule? obj) (fnmodule-get-field obj field))
    ;; TODO: add check for get-method implementations
    ((fnobj? obj) (fnobj-get-field obj field))
    (t (runtime-error "Object ~a has no gettable fields or indices" (->string obj)))))

(defun fnclass-get-field (obj key)
  (cond
    ((eq key name-sym) (fnclass-name obj))
    ((eq key fields-sym) (apply #'fnlist (fnclass-fields obj)))
    ((eq key constructor-sym) (fnclass-constructor obj))
    (t (runtime-error "get-field: object has no field named ~a" (->string key)))))

(defun fnmodule-get-field (obj key)
  (cond
    ((eq key name-sym) (fnmodule-name obj))
    ((eq key vars-sym) (fnmodule-vars obj))
    ((eq key macros-sym) (fnmodule-macros obj))
    (t (runtime-error "get-field: object has no field named ~a" (->string key)))))

(defun fnobj-get-field-cell (obj key)
  (gethash (sym-id key) (fnobj-contents obj)))

(defun fnobj-get-field (obj key)
  (unless (sym? key)
    (runtime-error "get-field: field name not a symbol: ~a" (->string key)))
  (aif (fnobj-get-field-cell obj key)
       (cell-value it)
       (runtime-error "get-field: object has no field named ~a" (->string key))))

;;; if
(defun eval-if (test then else)
  (let ((b (eval-code test)))
    (if (or (eq b fnnull)
            (eq b false))
        (eval-code else)
        (eval-code then))))

;;; let
(def-op "let" (bindings &rest body)
  (let* ((binding-pairs (group 2 (code-data bindings)))
         (*current-env* (init-env (mapcar $(code-data (car $))
                                          binding-pairs))))
    (mapc $(set-var (code-data (car $))
                    (eval-code (cadr $)))
          binding-pairs)
    (eval-body body)))

;;; quasiquote
;; TODO: add syntax checking for nested quaisquote, unquote, and unquote splicing (that is, check
;; that each one only has one argument).
(defun eval-quasiquote (code)
  ;; depth is the number of nested quasiquotes
  ;; TODO: rewrite so that src is a code object
  (labels ((us-form? (c)
             ;; Tell if c is an unquote-splice form
             (and (code-list? c)
                  (not (null (code-data c)))
                  (code-sym? (code-car c))
                  (eq (code-data (code-car c)) unquote-splice-sym)))
           (qqlist (lst level)
             (reduce $(if (us-form? $0)
                          (if (= level 1)
                              (fnappend (eval-code (code-cadr $0))
                                        $1)
                              (cons (qqtree $0 (+ level 1)) $1))
                          (cons (qqtree $0 level) $1))
                     lst
                     :from-end t
                     :initial-value empty))
           (qqtree (c level)
             (cond
               ;; atoms
               ((not (code-list? c))
                (code->fnvalue c))

               ;; empty list
               ((null (code-data c)) empty)

               ((code-sym? (code-car c))
                (let ((op (code-data (code-car c))))
                  (cond
                    ;; nested quasiquote
                    ((and (eq op quasiquote-sym)
                          (length= (code-data c) 2))
                     (qqlist (code-data c) (+ level 1)) )

                    ;; unquote
                    ((and (eq op unquote-sym)
                          (length= (code-data c) 2))
                     (if (= level 1)
                         (eval-code (code-cadr c))
                         (qqlist (code-data c) (- level 1))))

                    ;; if we find unquote-splice here, it means the list is malformed
                    ((and (eq op unquote-splice-sym)
                          (length= (code-data c) 2))
                     (if (= level 1)
                         (runtime-error "illegal unquote-splice in quasiquote")
                         (qqlist (code-data c) (- level 1))))

                    ;; nested lists
                    (t
                     (qqlist (code-data c) level)))))
               ;; nested lists
               (t (qqlist (code-data c) level)))))
    (if (code-list? code)
        (qqtree code 1)
        (code->fnvalue code))))

;;; quote
(defun eval-quote (code)
  (code->fnvalue code))

;;; set
(defun eval-set (place value-code)
  (cond
    ((code-sym? place) (set-var (code-data place) (eval-code value-code)))
    ((code-list? place)
     (cond ((eq (code-car place) get-field-sym)
            (set-field (eval-code (code-cadr place))
                       (mapcar #'eval-code (code-cddr place))
                       (eval-code value-code)))
           ((eq (code-car place) get-sym)
            (set-key (eval-code (code-cadr place))
                     (mapcar #'eval-code (code-cddr place))
                     (eval-code value-code)))
           (t (runtime-error "set: unrecognized set place"))))
    (t (runtime-error "set: unrecognized set place"))))

(defun set-field (obj0 fields value)
  (cond
    ((fnobj? obj0)
     (let ((obj1 (if (length< fields 2)
                     obj0
                     (reduce #'get-field
                             (butlast fields)
                             :initial-value obj0)))
           (f (car (last fields))))
       (aif (fnobj-get-field-cell obj1 f)
            (if (cell-mutable it)
                (setf (cell-value it) value)
                (runtime-error "set: object ~s field ~s is immutable"
                               (->string obj1)
                               (->string f)))
            (runtime-error "set: object ~s has no such field: ~s"
                               (->string obj1)
                               (->string f)))))
    (t (runtime-error "set: object ~s has no settable fields"
                      (->string obj0)))))

(defun set-key (obj0 keys value)
  (cond
    ((fnobj? obj0) (set-field obj0 keys value))
    (t (runtime-error "set: object ~s has no settable fields"
                      (->string obj0)))))
