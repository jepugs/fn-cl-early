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
  (:export :eval-ast :eval-code :eval-file :fnintern :fngensym :safe-add-global :*current-env*
           :*interpreter* :*interpreter-initialization-hook* :init-env :init-interpreter
           :runtime-error :runtime-warning))

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

;;;; interpreter state and name resolution

(defstruct interpreter
  (modules)
  (symtab (make-symtab)))

;;; these dynamic variables are used throughout to track lexical environment throughout the
;;; different evaluation functions
(defparameter *current-env* nil "Lexical environment used for evaluation")
(defparameter *interpreter* nil "Interpreter used for evaluation")

(defun current-module ()
  (env-module *current-env*))

;; function called to initialize the interpreter
(defvar *interpreter-initialization-hook* nil)

(defun find-module-by-name (sym)
  "Find a module corresponding to the provided symbol"
  (find-if $(eq (fnmodule-name $) sym) (interpreter-modules *interpreter*)))

(defun init-interpreter ()
  "Initialize an interpreter with built-in globals and symbols and a default module."
  (let* ((symtab (make-symtab))
         (mod (make-fnmodule :name (symtab-intern "__built-in" symtab)))
         (*interpreter* (make-interpreter :modules (list mod) :symtab symtab))
         (*current-env* (init-env nil nil mod)))
    ;; call the hook after setting *interpreter* and *current-env*
    (if *interpreter-initialization-hook*
        (funcall *interpreter-initialization-hook*))
    *interpreter*))

(defun init-env (syms &optional (parent *current-env*) (module nil))
  "Create an ENV with uninitialized value cells for each symbol in SYMS."
  (let ((table (make-hash-table :size (min (ceiling (* 1.5 (length syms)))
                                           10)))
        (module (or module
                    (if parent
                        (env-module parent)
                        (find-module-by-name (fnintern "__built-in"))))))
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

(defun fngensym (&optional (interpreter *interpreter*))
  "Get an uninterned symbol that still has a unique ID."
  (let ((st (interpreter-symtab interpreter)))
    (with-slots (fn.values::next-id) st
      (incf (slot-value st 'fn.values::next-id))
      (fn.values::make-sym :name (format nil "GENSYM-~a" fn.values::next-id)
                          :id fn.values::next-id))))

;; (setf *interpreter* (init-interpreter #'init-interpreter-hook))
;; (setf *current-env* (init-env nil nil (car (interpreter-modules *interpreter*))))

(defun safe-add-global (sym value &optional mutable function-name)
  "Add a global variable without trampling any definitions."
  (aif (get-module-cell (current-module) sym)
       (if (cell-mutable it)
           (progn
             (runtime-warning "~a: Redefining variable ~a"
                              function-name
                              (show sym))
             (setf (cell-value it) value))
           (runtime-error "~a: Attempt to redefine immutable variable ~a"
                          function-name
                          (show sym)))
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
       (runtime-error "set: Undefined variable ~s" (sym-name name)))

      ((not (cell-mutable cell))
       (runtime-error "set: Attempt to set immutable variable ~s"
                      (sym-name name)))

      (t (cell-mutable cell)
         (setf (cell-value cell) value)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; main evaluation functions

(defun eval-ast (a &optional (env *current-env*) (interpreter *interpreter*))
  "Convert an AST object to code, check it syntax errors, and evaluate it."
  (let* ((*interpreter* (or interpreter
                            (init-interpreter)))
         (*current-env* (or env
                            (init-env nil nil (find-module-by-name (fnintern "__built-in")))))
         (code (ast->code a (interpreter-symtab *interpreter*))))
    (validate-code code)
    (eval-code code env interpreter)))

(defun eval-code (c &optional (env *current-env*) (interpreter *interpreter*))
  "Evaluate a code object."
  (let* ((*interpreter* (or interpreter
                            (init-interpreter)))
         (*current-env* (or env
                            (init-env nil nil (find-module-by-name (fnintern "__built-in"))))))
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

(defun eval-body (body &optional fun-name)
  "Evaluate a list of code objects in order, returning the last expresison."
  (cond
    ((null body) (runtime-error "~aNo expressions in body"
                                (if fun-name
                                    (concatenate 'string fun-name ": ")
                                    "")))
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
      ((eq op if-sym)
       (eval-if (car args) (cadr args) (caddr args)))
      ((eq op import-sym)
       (eval-import (code-data (car args)) (cdr args)))
      ((eq op let-sym)
       (eval-let (car args) (cdr args)))
      ((eq op quasiquote-sym)
       (eval-quasiquote (car args)))
      ((eq op quote-sym)
       (eval-quote (car args)))
      ((eq op set-sym)
       (eval-set (car args) (cadr args)))
      ((eq op unquote-sym)
       (runtime-error "unquote: unquote outside of quasiquote"))
      ((eq op unquote-splice-sym)
       (runtime-error "unquote-splice: unquote-splice outside of quasiquote"))
      ((code-dotted-get? (code-car c))
       (eval-get-op-list c))
      ((not (sym? op)) (eval-funcall c))
      ;; check for macros
      (t (aif (get-macro *current-env* op)
              (let ((code (expand-macro it c)))
                (validate-code code)
                (eval-code code))
              (eval-funcall c))))))

(defun var-module (obj)
  (aif (get-module-cell *current-env* obj)
       (if (fnmodule? (cell-value it))
           (cell-value it)
           nil)
       nil))

(defun dotted-get-macro (root keys)
  ;; TODO: adjust to allow descending macros with multiple keys
  (let ((next-mod (aif (get-module-cell (current-module) root)
                       (cell-value it)
                       nil)))
    (if (and (fnmodule? next-mod)
             (= (length keys) 1))
        (get-module-macro next-mod (car keys))
        nil)))

(defun eval-get-op-list (c)
  "Evaluate a list where the head is a dotted get form."
  (let ((root (dotted-get-root (code-car c)))
        (keys (dotted-get-keys (code-car c))))
    (aif (dotted-get-macro root keys)
         (let ((code (expand-macro it c)))
           (validate-code code)
           (eval-code code))
         (eval-funcall c))))

(defun params-alist (param-list args outer-env &optional op-name)
  "Create an ALIST matching parameter symbols to argument values. This function generates runtime
 errors if the provided argument list doesn't match the parameter list. As such it may be used for
 error checking."
  (with-slots (pos keyword vari) param-list
    (let ((err-pre (if op-name
                       (concatenate 'string op-name ": ")
                       "")))
      (labels ((get-optional (p)
                 (if (cdr p)
                     (eval-code (cdr p) outer-env)
                     (runtime-error "~aToo few arguments" err-pre)))
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
                         (runtime-error "~aMalformed keyword argument" err-pre))
                       (when (null v)
                         (runtime-error "~aOdd number of keyword arguments" err-pre))
                       (if (assoc k keyword :test #'equal)
                           (bind-keyword (cons (cons k v) acc)
                                         vari-acc
                                         (cddr args))
                           (if vari
                               ;; vari-acc is built backwards
                               (bind-keyword acc
                                             (cons v (cons k vari-acc))
                                             (cddr args))
                               (runtime-error "~aUnrecognized keyword argument ~s"
                                              err-pre
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
                         (runtime-error "~aToo many arguments" err-pre)
                         nil))))
        (bind-positional nil pos args)))))

(defun bind-params (param-list args outer-env &optional op-name)
  "Extend OUTER-ENV with variable bindings created by PARAM-LIST and ARGS."
  (let* ((binding-alist (remove-if $(eq (car $) wildcard-sym)
                                   (params-alist param-list args outer-env op-name)))
         (*current-env* (init-env (mapcar #'car binding-alist) outer-env)))
    (mapc $(set-var (car $) (cdr $)) binding-alist)
    *current-env*))

(defun expand-macro (macro-fun c)
  (with-slots (filename line column) (code-origin c)
    ;; TODO: change so that the origin looks better with dot syntax
    (let* ((origin (make-origin :filename filename
                                :line line
                                :column column
                                :macro (format nil "~a" (code-data (code-car c))))) 
           (args (mapcar #'code->fnvalue (code-cdr c))))
      (fnvalue->code (call-fun macro-fun args) origin))))

(defun call-fun (fnfun args)
  (with-slots (params body closure) fnfun
    (if (functionp body)
        (funcall body args)
        (let ((*current-env* (bind-params params args closure)))
          (eval-body body)))))

(defun call-fnmethod (m args)
  (with-slots (name params dispatch-params default-impl) m
    (let* ((new-env (bind-params params args *current-env* name))
           (types (mapcar $(get-class-of (cell-value (get-cell new-env $)))
                          dispatch-params)))
      (aif (get-impl m types)
           (call-fun it args)
           (if default-impl
               (call-fun default-impl args)
               (runtime-error "~aMethod not implemented on types (~{~a~^ ~})."
                              (if name
                                  (concatenate 'string name ": ")
                                  "")
                              (mapcar $(show (fnclass-name $)) types)))))))

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
      (runtime-error "apply: Last argument must be a list"))
    (let ((arg-list (reduce #'cons args :from-end t)))
      (rplacd (last arg-list) nil)
      (call-obj op arg-list))))

;;; class-of
(defun eval-class-of (arg-code)
  (or (get-class-of (eval-code arg-code))
      (runtime-error "class-of: Object has unknown class")))

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
           (runtime-error "not a class: ~a" (show sym)))
       (runtime-error "no such class: ~a" (show sym))))

;;; cond
(defun eval-cond (body)
  (let ((clauses (group 2 body)))
    (rlambda (src) (clauses)
      (if src
          (let ((x (eval-code (caar src))))
            (if (or (eq x fnnull)
                    (eq x false))
                (recur (cdr src))
                (eval-code (cadar src))))
          fnnull))))

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

(defun eval-function-def (name params-code body)
  (let ((fun (make-fnfun :params (code->param-list params-code)
                         :body body
                         :closure *current-env*)))
    (safe-add-global name fun nil "def")
    fun))

(defun eval-method-def (name class-syms params-code body)
  (let* ((m (aif (get-cell *current-env* name) (cell-value it) nil))
         (params (code->param-list params-code)))
    (unless m
      (runtime-error "def: No such method ~s." (show name)))
    (unless (fnmethod? m)
      (runtime-error "def: ~s is not a method." (show name)))
    (unless (eql (length class-syms) (length (fnmethod-dispatch-params m)))
      (runtime-error "def: Incorrect number of dispatch-params. dispatch-params for ~s are \"(~{~a~^ ~})\"."
                     (show name)
                     (fnmethod-dispatch-params m)))
    (unless (param-list-eqv (fnmethod-params m) params)
      (runtime-error
       "def: Mismatched method parameter list. ~s parameters are \"(~{~a~^ ~})\"."
       (show name)
       (mapcar #'show (fnmethod-params m))))
    (let ((types (mapcar $(class-by-name $) class-syms)))
      (if (get-impl m types)
          (runtime-error "def: Method ~a already defined on types \"(~{~a~^ ~})\"."
                         (show name)
                         (mapcar #'show class-syms))
          (set-impl m types (make-fnfun :params params
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
  (let ((m (make-fnmethod :name (sym-name name)
                          :params (code->param-list params)
                          :dispatch-params (mapcar #'code-data dispatch-params))))
    (safe-add-global name m nil "defmethod")
    m))

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
    ((fnclass? obj) (fnclass-get-field obj key))
    ((fnmodule? obj) (fnmodule-get obj key))
    ((string? obj) (fnstring-get obj key))
    ;; TODO: add check for get-method implementations
    ((fnobj? obj) (fnobj-get-field obj key))
    (t (runtime-error "Object ~s has no gettable fields or indices" (show obj)))))

(defun fnmodule-get (obj key)
  "Gets a variable from a module by name"
  (unless (sym? key)
    (runtime-error "get: Key for module ~s is not a symbol: ~s"
                   (show (fnmodule-name obj))
                   (show key)))
  (aif (get-module-cell obj key)
       (cell-value it)
       (runtime-error "get: Module ~s contains no variable named ~s"
                      (show (fnmodule-name obj))
                      (show key))))

(defun fnstring-get (obj key)
  (unless (num? key)
    (runtime-error "get: String index is not a number: ~a" (show key)))
  (unless (num-int? key)
    (runtime-error "get: String index is not an integer: ~a" (show key)))
  (unless (>= 0 key)
    (runtime-error "get: String index is negative: ~a" (show key)))
  (if (> key (length obj))
      (runtime-error "get: String index out of bounds: ~a" (show key))
      (aref obj (truncate key))))

(defun get-field (obj field)
  (cond
    ((fnclass? obj) (fnclass-get-field obj field))
    ((fnmodule? obj) (fnmodule-get-field obj field))
    ;; TODO: add check for get-method implementations
    ((fnobj? obj) (fnobj-get-field obj field))
    (t (runtime-error "get: Object has no gettable fields or indices"))))

(defun fnclass-get-field (obj key)
  (cond
    ((eq key name-sym) (fnclass-name obj))
    ((eq key fields-sym) (apply #'fnlist (fnclass-fields obj)))
    ((eq key constructor-sym) (fnclass-constructor obj))
    (t (runtime-error "get: Object has no field named ~a" (show key)))))

(defun fnmodule-get-field (obj key)
  (cond
    ((eq key name-sym) (fnmodule-name obj))
    ((eq key vars-sym) (fnmodule-vars obj))
    ((eq key macros-sym) (fnmodule-macros obj))
    (t (runtime-error "get: Object has no field named ~a" (show key)))))

(defun fnobj-get-field-cell (obj key)
  (gethash (sym-id key) (fnobj-contents obj)))

(defun fnobj-get-field (obj key)
  (unless (sym? key)
    (runtime-error "get: Field name not a symbol: ~a" (show key)))
  (aif (fnobj-get-field-cell obj key)
       (cell-value it)
       (runtime-error "get: Object has no field named ~a" (show key))))

;;; if
(defun eval-if (test then else)
  (let ((b (eval-code test)))
    (if (or (eq b fnnull)
            (eq b false))
        (eval-code else)
        (eval-code then))))

;;; import
(defun eval-import (sym as-args)
  (let ((new-name (if as-args
                      (code-data (cadr as-args))
                      sym)))
    ;; check if the symbol is already bound
    (aif (get-module-cell (current-module) new-name)
         ;; if we're reloading the same module as was in that variable before, that's ok, so we
         ;; set the value cell to NULL. Otherwise, we let SAFE-ADD-GLOBAL binding semantics
         ;; decide what to do.
         (when (and (fnmodule? (cell-value it))
                    (eq (fnmodule-name (cell-value it)) sym))
           (runtime-warning "import: Reloading module ~s" (sym-name sym))
           ;; FIXME: deleting the value cell probably will cause optimization bugs
           (add-global-cell *current-env* new-name nil))
         nil)
    (let ((mod (init-import-module sym (find-module-by-name (fnintern "__built-in")))))
      ;; load the module code
      (unless mod
        (runtime-error "import: Couldn't find module by name ~s" (sym-name sym)))
      (handler-case (let ((*current-env* (init-env nil nil mod)))
                      (eval-file (fnmodule-loaded-from mod)))
        (fn-error (x)
          (runtime-error "import: Error importing ~s:~%~a"
                         (sym-name sym)
                         x)))
      ;; set the variable and add to the modules list
      (push mod (interpreter-modules *interpreter*))
      (safe-add-global new-name mod nil "import")
      mod)))

(defun eval-file (path)
  (handler-case
      (with-open-file (in path :direction :input)
        (->> (fn.scanner:scan in path)
          (fn.parser:parse)
          (mapc #'eval-ast)))
    (sb-ext:file-does-not-exist ()
      (runtime-error "Error loading file: file ~s does not exist" path)))
  fnnull)

;;; let
(defun eval-let (bindings body)
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
                         (runtime-error "quasiquote: Illegal unquote-splice")
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
    ((code-sym? place) (set-var (code-data place)
                                (eval-code value-code)))
    ((code-get-expr? place)
     (let* ((root-obj (eval-code (code-cadr place)))
            (keys (code-cddr place)))
       (set-key root-obj
                (mapcar #'eval-code keys)
                (eval-code value-code))))
    (t (runtime-error "set: Unrecognized set place"))))

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
                (runtime-error "set: Object ~s field ~s is immutable"
                               (show obj1)
                               (show f)))
            (runtime-error "set: Object ~s has no such field: ~s"
                               (show obj1)
                               (show f)))))
    (t (runtime-error "set: Object ~s has no settable fields"
                      (show obj0)))))

(defun set-key (obj0 keys value)
  (cond
    ((fnobj? obj0) (set-field obj0 keys value))
    (t (runtime-error "set: Object ~s has no settable fields"
                      (show obj0)))))
