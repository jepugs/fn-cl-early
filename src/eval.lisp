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
    (with-gensyms (x)
      `(handler-case (progn ,@body)
         (runtime-error (,x)
           (princ ,code)
           (fn-error (code-origin ,code)
                     (slot-value ,x 'message))))))

  (defmacro runtime-error (format-string &rest format-args)
    `(error 'runtime-error
            :message (format nil ,format-string ,@format-args))))


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
  (defun-builtin "String" args
    (apply #'fnstring args))
  (defun-builtin "List" args
    (apply #'fnlist args))
  (defun-builtin "exit" args
    (funcall #'sb-ext:exit
             :code (if args (floor (car args)) 0)))

  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; interpreter state

(defstruct interpreter
  (modules)
  (symtab (make-symtab)))

(defun fnintern (name interpreter)
  "Get an internal symbol using INTERPRETER's symbol table."
  (symtab-intern name (interpreter-symtab interpreter)))

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

(defparameter *interpreter* (init-interpreter)
  "Interpreter used for evaluation")
(defparameter *current-env* (init-env nil nil (car (interpreter-modules *interpreter*)))
  "Lexical environment used for evaluation")

(defun search-for-module (str)
  (runtime-error "Could not find module ~a" str))

(defun find-module (sym)
  (aif (find-if $(eq sym (fnmodule-name $))
                (interpreter-modules *interpreter*))
       (cdr it)
       (search-for-module (sym-name sym))))

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

(defun current-module ()
  (env-module *current-env*))

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

(defun eval-list (c)
  (let* ((op (code-data (code-car c)))
         (args (code-cdr c)))
    (cond
      ((eq op apply-sym) (eval-apply args))
      ((eq op def-sym) (eval-def (car args) (cdr args)))
      ((not (sym? op)) (eval-funcall c))
      (t (aif (assoc (sym-name op)
                     special-op-dispatch
                     :test #'equal)
              (funcall (cdr it) c *current-env* *interpreter*)
              (aif (get-macro *current-env* op)
                   (runtime-error "Macros aren't implemented")
                   (eval-funcall c)))))))

(defun eval-body (body)
  "Evaluate a list of code objects in order, returning the last expresison."
  (cond
    ((null body) (runtime-error "No expressions in body"))
    ((length= body 1) (eval-code (car body)))
    (t (eval-code (car body))
       (eval-body (cdr body)))))

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

(defun call-fun (fnfun args)
  (with-slots (params body closure) fnfun
    (if (functionp body)
        (funcall body args)
        (let ((*current-env* (bind-params params args closure *interpreter*)))
          (eval-body body)))))

(defun eval-funcall (c)
  "Evaluate a function call."
  (let ((args (mapcar #'eval-code
                      (code-cdr c)))
        (op (eval-code (code-car c))))
    (unless (fnfun? op)
      (runtime-error "Operator is not a function"))
    (call-fun op args)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; special operators

(defun eval-apply (arg-exprs)
  (let ((fun (eval-code (car arg-exprs)))
        (args (mapcar $(eval-code $) (cdr arg-exprs))))
    (unless (fnlist? (car (last args)))
      (runtime-error "apply: last argument must be a list"))
    (let ((arg-list (reduce #'cons args :from-end t)))
      (rplacd (last arg-list) nil)
      (call-fun fun arg-list))))

;; TODO: right now, new definitions silently clobber old ones. This behavior is useful for testing,
;; but dangerous in practice, so it should at least emit a warning.
(defun eval-def (name-or-proto value-or-body)
  (if (code-list? name-or-proto)
      (let ((name (code-car name-or-proto))
            (params (code-cdr name-or-proto)))
        (eval-function-def (code-data name) params value-or-body))
      (let ((v (eval-code (car value-or-body))))
        (add-global-cell *current-env*
                         (code-data name-or-proto)
                         (make-cell :value v :mutable nil))
        v)))

;; (defun eval-def )

(defun eval-function-def (name params body)
  (let ((fun (make-fnfun :params (code->param-list params)
                         :body body
                         :closure *current-env*)))
    (add-global-cell *current-env*
                     name
                     (make-cell :value fun))
    fun))

(def-op "defvar" (name value)
  (let ((v (eval-code value)))
    (add-global-cell *current-env*
                     (code-data name)
                     (make-cell :value v :mutable t))
    v))


(def-op "set" (place value)
  (cond
    ((code-sym? place) (set-var (code-data place)
                                (eval-code value env interpreter)))
    (t (runtime-error c "Unrecognized set place"))))

(defun code-quoted-sym? (c)
  (with-slots (data) c
    (and (listp data)
         (length= data 2)
         (every #'code-sym? data)
         (string= quot-sym-name (code-sym-name (car data))))))

(defun code->param-list (lst)
  "Takes a list of code objects and generates a param-list"
  (let* ((non-vari
          (take-while $(not (and (code-sym? $)
                                 (string= (code-sym-name $) "&")))
                      lst))
         (pos (remove-if-not $(or (and (code-sym? $)
                                       (not (string= (code-sym-name $) "&")))
                                  (and (code-list? $)
                                       (code-sym? (code-car $))
                                       (not (code-quoted-sym? $))))
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

(def-op "fn" (params &rest body)
  (make-fnfun :params (code->param-list (code-data params))
              :body body
              :closure *current-env*))

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
     (let ((op (code-sym-name (code-car expr))))
       (cond
         ((and (string= op quasiquot-sym-name)
               (length= (code-data expr) 2))
          (find-$-qquote (code-cadr expr) (+ level 1)))
         ((and (or (string= op unquot-sym-name)
                   (string= op unquot-splice-sym-name))
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
     (let ((op (code-sym-name (code-car expr))))
       (cond
         ((string= op quasiquot-sym-name)
          (find-$-qquote (code-cadr expr) 1))
         ((string= op quot-sym-name) nil)
         ((string= op dollar-sym-name) nil)
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

(def-op "dollar-fn" (expr)
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
                 collect (cons (fnintern (format nil "$~a" i)
                                         interpreter)
                               nil)))
         (params (make-param-list :pos pos
                                  :vari (and has-vari
                                             (fnintern "$&" interpreter)))))
    (make-fnfun :body
                (lambda (args)
                  (let ((env0 (bind-params params args env interpreter)))
                    ;; add the alternative symbol for the first arg
                    (if (>= max-id 0)
                        (add-cell env0
                                  (fnintern "$" interpreter)
                                  (get-cell *current-env* (fnintern "$0" interpreter))))
                    (eval-code expr env0 interpreter))))))

(def-op "if" (test then else)
  (let ((b (eval-code test env interpreter)))
    (if (or (eq b fnnull)
            (eq b false))
        (eval-code else env interpreter)
        (eval-code then env interpreter))))

(def-op "cond" (&rest body)
  (let ((clauses (group 2 body)))
    (rlambda (src) (clauses)
      (when (null src)
        (runtime-error "every cond clause failed"))
      (let ((x (eval-code (caar src) env interpreter)))
        (if (or (eq x fnnull)
                (eq x false))
            (recur (cdr clauses))
            (eval-code (cadar src) env interpreter))))))

(def-op "quote" (code)
  (code->fnvalue code))

(def-op "quasiquote" (code)
  (eval-quasiquote code env interpreter))

(def-op "unquote" (code)
  (declare (ignore code))
  (runtime-error "unquote outside of quasiquote"))

(def-op "unquote-splice" (code)
  (declare (ignore code))
  (runtime-error "unquote-splice outside of quasiquote"))

;; TODO: add syntax checking for nested quaisquote, unquote, and unquote splicing (that is, check
;; that each one only has one argument).
(defun eval-quasiquote (code env interpreter)
  (let ((qq (fnintern quasiquot-sym-name interpreter))
        (uq (fnintern unquot-sym-name interpreter))
        (us (fnintern unquot-splice-sym-name interpreter)))
    ;; depth is the number of nested quasiquotes
    ;; TODO: rewrite so that src is a code object
    (labels ((us-form? (c)
               ;; Tell if c is an unquote-splice form
               (and (code-list? c)
                    (not (null (code-data c)))
                    (code-sym? (code-car c))
                    (eq (code-data (code-car c)) us)))
             (qqlist (lst level)
               (reduce $(if (us-form? $0)
                            (if (= level 1)
                                (fnappend (eval-code (code-cadr $0)
                                                     env
                                                     interpreter)
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
                      ((and (eq op qq)
                            (length= (code-data c) 2))
                       (qqlist (code-data c) (+ level 1)) )

                      ;; unquote
                      ((and (eq op uq)
                            (length= (code-data c) 2))
                       (if (= level 1)
                           (eval-code (code-cadr c) env interpreter)
                           (qqlist (code-data c) (- level 1))))

                      ;; if we find unquote-splice here, it means the list is malformed
                      ((and (eq op us)
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
          (code->fnvalue code)))))




(def-op "do" (expr0 &rest body)
  (eval-body (cons expr0 body)))

(def-op "let" (bindings &rest body)
  (let* ((binding-pairs (group 2 (code-data bindings)))
         (*current-env* (init-env (mapcar $(code-data (car $))
                                          binding-pairs))))
    (mapc $(set-var (code-data (car $))
                    (eval-code (cadr $)))
          binding-pairs)
    (eval-body body)))
