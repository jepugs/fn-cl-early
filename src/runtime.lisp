;;;; runtime.lisp -- runtime state

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

(defpackage :fn.runtime
  (:documentation "runtime state")
  (:use :cl :fn.util :fn.ast :fn.values :fn.code)
  (:export
   ;; runtime error
   :runtime-error :handling-runtime-errors :runtime-warning
   ;; cell struct
   :cell :make-cell :cell-value :cell-mutable
   ;; env struct
   :env :make-env :env-table :env-module :env-call-stack :env-parent
   ;; fnmodule struct
   :fnmodule :make-fnmodule :fnmodule? :fnmodule-name :fnmodule-filename :fnmodule-vars
   :fnmodule-macros
   ;; getting and setting vars/macros
   :module-cell :module-var :module-macro :env-cell :env-var :env-macro :env-class
   :module-import-syms
   ;; call frame struct
   :call-frame :make-call-frame :call-frame-origin :call-frame-name :show-stack-trace
   ;; runtime structure
   :runtime :make-runtime :runtime-built-in-module :runtime-modules :runtime-symtab
   ;; runtime state
   :*current-env* :*runtime* :fnintern :fngensym :init-env :extend-env
   ;; dotted get
   :code-dotted-get? :dotted-get-root :dotted-get-keys :dotted-get-cell :dotted-get-macro
   ;; module search/init
   :*module-search-path* :search-for-module :current-module :find-module-by-sym :init-import-module
   :init-module
   ;; variable mutation
   :safe-add-global :safe-add-global-macro :safe-set-cell-value :safe-set-var))

(in-package :fn.runtime)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; runtime errors and warnings

;;; runtime errors are handled by the eval functions so that the offending line of code can be
;;; implicated

(eval-when (:load-toplevel :compile-toplevel :execute)
  (define-condition runtime-error (error)
    ((message :initarg :message)
     (call-stack :initarg :call-stack)))
  (define-condition runtime-warning (warning)
    ((message :initarg :message)
     (call-stack :initarg :call-stack)))

  (defmacro handling-runtime-errors (code &body body)
    "Converts runtime errors to fn errors"
    (with-gensyms (x code-sym)
      `(let ((,code-sym ,code))
         (handler-case (progn ,@body)
           (runtime-error (,x)
             (fn-error (code-origin ,code-sym)
                       (strcat "Runtime error: "
                               (slot-value ,x 'message)
                               (show-stack-trace (slot-value ,x 'call-stack)))))))))

  (defmacro runtime-error (format-string &rest format-args)
    "Create a runtime error "
    `(error 'runtime-error
            :message (format nil ,format-string ,@format-args)
            :call-stack (env-call-stack *current-env*)))

  ;; one day this will be implemented
  (defmacro runtime-warning (format-string &rest format-args)
    (declare (ignore format-string format-args))
    nil))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; modules and lexical environments

;;; a generic memory cell. This wraps an fn value so that modifications to the value in the cell
;;; will be seen across all closures, etc containing it
(defstruct (cell :predicate :copier)
  (value fnnull)
  (mutable nil :read-only t))

(defstruct (env :predicate :copier)
  ;; hash table mapping symbol IDs onto cells
  (table (make-hash-table :test 'eql))
  ;; module used for global variable/macro resolution
  (module nil :type fnmodule)
  ;; list of call frame objects
  (call-stack nil :type list)
  ;; surrounding lexical environment
  (parent nil))

(defstruct (fnmodule (:predicate fnmodule?) :copier)
  ;; symbol naming this module
  (name nil :type sym)
  ;; module source code filename. NIL for the __built-in and __interactive modules.
  (filename nil)
  ;; hash table from symbol ID's (which are fixnums) to CELL objects
  (vars (make-hash-table) :type hash-table)
  ;; hash table from symbol ID's
  (macros (make-hash-table) :type hash-table))

;; modules and environments usually contain references back to the global environment (e.g. in
;; function closures), so we have to do this to stop infinite print loops
(defmethod print-object ((object env) stream)
  (with-slots (table module parent) object
    (format stream "<ENV:module=~s>" (sym-name (fnmodule-name module)))))
(defmethod print-object ((object fnmodule) stream)
  (format stream "<MODULE:~s>" (sym-name (fnmodule-name object))))

(declaim (inline module-cell module-var module-macro env-cell env-var env-macro env-class
                 env-module))

;; module/environment functions
(defun module-cell (mod sym)
  "Get a variable cell. Returns NIL on if there is no such variable."
  (declare (type fnmodule mod)
           (type sym sym))
  (gethash (sym-id sym) (fnmodule-vars mod)))

(defsetf module-cell (mod sym) (cell)
  "Set a variable cell."
  `(setf (gethash (sym-id ,sym) (fnmodule-vars ,mod)) ,cell))

(defun module-var (mod sym)
  "Get the value of a variable. Returns NIL if there is no such variable."
  (declare (type fnmodule mod)
           (type sym sym))
  (aif (module-cell mod sym)
       (cell-value it)
       nil))

(defun module-macro (mod sym)
  "Get the macro function bound to SYM in module MOD."
  (declare (type fnmodule mod)
           (type sym sym))
  (gethash (sym-id sym) (fnmodule-macros mod)))

(defsetf module-macro (mod sym) (value)
  "Set the macro associated with SYM in the provided module."
  `(setf (gethash (sym-id ,sym) (fnmodule-macros ,mod)) ,value))

(defun env-cell (env sym)
  "Get a variable cell. Returns NIL on if there is no such variable."
  (declare (ftype (function (env sym) cell) env-cell))
  (or (gethash (sym-id sym) (env-table env))
      (aif (env-parent env)
           (env-cell it sym)
           (gethash (sym-id sym) (fnmodule-vars (env-module env))))))

(defsetf env-cell (env sym) (value)
  `(setf (gethash (sym-id ,sym)
                  (env-table ,env))
         ,value))

(defun env-var (env sym)
  "Get the value of a variable. Returns NIL if there is no such variable."
  (declare (type env env)
           (type sym sym))
  (aif (env-cell env sym)
       (cell-value it)
       nil))

(defsetf env-var (env sym) (value)
  "Set a variable in an environment. Doesn't check for mutability. This will silently fail if there
 is no existing CELL object."
  (with-gensyms (cell)
    `(let ((,cell (env-cell ,env ,sym)))
       (if ,cell
           (setf (cell-value ,cell) ,value)
           nil))))

(defun env-macro (env sym)
  "Get the macro function bound to SYM in the provided environment."
  (declare (type env env)
           (type sym sym))
  ;; for now there are no local macros
  (module-macro (env-module env) sym))

(defun env-class (env sym)
  "Check if SYM is bound to an FNCLASS object. Returns the object on success and NIL on failure."
  (aif (env-cell env sym)
       (if (fnclass? (cell-value it))
           (cell-value it)
           nil)
       nil))

(defun module-import-syms (dest src &optional (syms nil syms-p))
  "Copy variables and macros from module SRC into DEST. SYMS is a list of symbols to copy. If SYMS
 is not specified, every symbol is copied."
  (if syms-p
      (let ((sym-ids (mapcar #'sym-id syms)))
        (maphash $(if (member $0 sym-ids :test 'eql)
                      (setf (module-cell dest $0) $1))
                 (fnmodule-vars src))
        (maphash $(if (member $0 sym-ids :test 'eql)
                      (setf (module-macro dest $0) $1))
                 (fnmodule-macros src)))
      (progn
        (maphash $(setf (module-cell dest $0) $1)
                 (fnmodule-vars src))
        (maphash $(setf (module-macro dest $0) $1)
                 (fnmodule-macros src))))
  dest)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; call stack

(defstruct (call-frame :copier :predicate)
  ;; origin of the code object for the function call
  (origin nil :type origin :read-only t)
  ;; string describing the operator that made the call
  (object nil :read-only t))

(defun show-stack-trace (call-stack &optional (max-depth 5))
  "Format a stack trace as a multi-line string that starts with a newline."
  (format nil
          "~{~%    in call to ~a at ~a~}~a"
          (mapcan $(list (show (call-frame-object $))
                         (origin->string (call-frame-origin $)))
                  (take max-depth call-stack))
          ;; print ellipses if the call stack is too long
          (if (length< call-stack (+ max-depth 1))
              ""
              "...")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; runtime state

(defstruct (runtime :predicate :copier)
  ;; the built-in module is used as a template for creating new modules. It does not appear in the
  ;; MODULES list.
  (built-in-module (make-fnmodule :name built-in-module-sym) :type fnmodule)
  ;; list of all modules
  (modules nil :type list)
  ;; symbol table for the runtime
  (symtab (make-symtab) :type symtab))

;;; these dynamic variables are used throughout the evaluator so that we don't have to pass the
;;; environment and runtime as arguments to everything
(defparameter *current-env* nil "Lexical environment used for evaluation")
(defparameter *runtime* nil "Runtime used for evaluation")

(defun fnintern (str &optional (runtime *runtime*))
  "Get an internal symbol using RUNTIME's symbol table."
  (symtab-intern str (runtime-symtab runtime)))

(defun fngensym (&optional (runtime *runtime*))
  "Get an uninterned symbol that still has a unique ID using RUNTIME's symbol table."
  (let ((st (runtime-symtab runtime)))
    (with-slots (fn.values::next-id) st
      (incf (slot-value st 'fn.values::next-id))
      (fn.values::make-sym :name (format nil "#<GENSYM:~a>" fn.values::next-id)
                           :id fn.values::next-id))))

(defun init-env (&optional module)
  "Initialize an environment object with an empty call stack."
  (make-env :module (or module (runtime-built-in-module *runtime*))))

(defun extend-env (syms &key (parent *current-env*) call-stack module)
  "Create a new environment as an extension of PARENT. The MODULE and CALL-STACK are inherited from
 the parent by default, but may be specified manually via the respective keyword arguments."
  (let* ((table (make-hash-table))
         (res (make-env :table table
                        :module (or module (env-module parent))
                        :call-stack (or call-stack (env-call-stack parent))
                        :parent parent)))
    (mapc $(setf (gethash (sym-id $) table) (make-cell :mutable t)) syms)
    res))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; getting values of dotted get expressions

;;; A dotted get expression is one where:
;;; - there is only one key, and that key is a quoted symbol
;;; - the first argument is either a symbol or another dotted get expression
;;; in other words, an expression that can be expressed with dot syntax e.g. a.b.c

;;; Dotted get expressions are treated specially in certain contexts. For instance, dotted get
;;; expressions may refer to macros when they appear as operators. Method definitions allow dotted
;;; gets in both

;;; the function CODE-DOTTED-GET? is in code.lisp
(defun code-get-expr? (c)
  "Tell if an expression is a valid get expression"
  (and (code-list? c)
       (eq (code-data (code-car c)) get-sym)
       (not (length< (code-cdr c) 2))))

(defun code-dotted-get? (c)
  "Tell if an expression is a dotted get (a get expression which could have been produced solely
 using dot syntax)."
  (and (code-get-expr? c)
       (length= (code-data c) 3)
       (code-quoted-sym? (code-caddr c))
       (or (code-sym? (code-cadr c))
           (code-dotted-get? (code-cadr c)))))

(defun dotted-get-root (c)
  "Get the sym corresponding to the root object of a dotted get. Assumes C satisfies
 CODE-DOTTED-GET."
  (if (code-list? (code-cadr c))
      (dotted-get-root (code-cadr c))
      (code-data (code-cadr c))))

(defun dotted-get-keys (c)
  "Get a list of the keys of a dotted get in the order they would be applied."
  (rlambda (acc data) (nil (code-data c))
    (if (code-dotted-get? (cadr data))
        (recur (cons (code-data (code-cadr (caddr data))) acc)
               (cadr data))
        (reverse acc))))

(defun dotted-get-cell (c)
  "Takes a code object containing a dotted get expression. If the expression corresponds to a
 variable in a module, returns the value cell. Otherwise, returns NIL."
  (rlambda (v k*) ((env-cell *current-env* (dotted-get-root c))
                   (dotted-get-keys c))
    (cond
      ((null v) nil)
      ((null k*) v)
      ((fnmodule? (cell-value v))
       (recur (module-cell (cell-value v) (car k*))
              (cdr k*)))
      (t nil))))

(defun dotted-get-macro (c)
  "Takes a code object containing a dotted get expression. If the expression corresponds to a
 macro in a module, returns the value cell. Otherwise, returns NIL."
  (rlambda (v k*) ((env-cell *current-env* (dotted-get-root c))
                   (dotted-get-keys c))
    (cond
      ((null v) nil)
      ((null k*) v)
      ((fnmodule? (cell-value v))
       ;; different behavior for the last key
       (if (null (cdr k*))
           (module-macro (cell-value v) (car k*))
           (recur (module-cell (cell-value v) (car k*))
                  (cdr k*))))
      (t nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; module search and initialization

(defvar *module-search-path* '("./" "/usr/local/lib/fn/modules/" "/usr/lib/fn/modules/"))

;;; FIXME: non-portable usage of SBCL's DIRECTORY function
;; also, notes so I don't forget: the :TYPE field is used _ONLY_ for the last extension of the
;; filename, e.g. code.test.lisp has :NAME "code.test" and :TYPE "lisp".
(defun search-for-module (name)
  "Find a module source file given its name as a string. Searches each directory in
 *MODULE-SEARCH-PATH* for a file named NAME.fn."
  (some (lambda (dir)
          (aif (directory (make-pathname :directory (list :relative dir)
                                         :name name
                                         :type "fn"))
               (car it)
               nil))
        *module-search-path*))

(defun current-module ()
  "Get the current module."
  (env-module *current-env*))

(defun find-module-by-sym (sym)
  "Search the current runtime a module whose name is the provided symbol."
  (find-if $(eq (fnmodule-name $) sym) (runtime-modules *runtime*)))

(defun init-import-module (sym)
  "Given a symbol, search for a module and then call INIT-MODULE with the appropriate name and
 filename. Returns NIL on failure."
  (aif (search-for-module (sym-name sym))
       (init-module sym it)
       nil))

(defun init-module (name &optional (filename nil))
  "Initialize a module with the provided NAME and FILENAME and bindings copied from the built-in
 module."
  (with-slots (vars macros) (runtime-built-in-module *runtime*)
    (make-fnmodule :name name
                   :filename filename
                   ;; this is alright because we copy the CELLs rather than directly copying the
                   ;; variable values
                   :vars (copy-ht vars)
                   ;; this is alright because macros are immutable
                   :macros (copy-ht macros))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; variable mutation with error reporitng

(defun safe-add-global (sym value &optional mutable op-str)
  "Add a global variable to the current module, but emit an error if it's already bound immutably."
  (aif (module-cell (current-module) sym)
       (if (cell-mutable it)
           (progn
             (runtime-warning "~a: Redefining variable ~a"
                              op-str
                              (show sym))
             (setf (cell-value it) value))
           (runtime-error "~a: Attempt to redefine immutable variable ~a"
                          op-str
                          (show sym)))
       (setf (module-cell (current-module) sym)
             (make-cell :value value
                        :mutable mutable))))

(defun safe-add-global-macro (sym value &optional op-str)
  "Add a global macro to the current module, but emit an error if it's already bound."
  (let ((mod (current-module)))
    (aif (module-macro mod sym)
         (runtime-error "~a: Attempt to redefine macro ~a" op-str (sym-name sym))
         (setf (module-macro mod sym) value))))

(defun safe-set-cell-value (cell value &optional op-str)
  "Set the value of a cell, but emit an error if it's immutable"
  (if (cell-mutable cell)
      (setf (cell-value cell) value)
      (runtime-error "~a: Variable is immutable" op-str)))

(defun safe-set-var (sym-or-dotted-get value &optional op-str)
  "Set the value of a variable, but emit an error if it doesn't exist of it it's immutable.
 SYM-OR-DOTTED-GET is either a normal SYM object (not a code sym) or a code list containing a dotted
 get expression."
  (if (code-dotted-get? sym-or-dotted-get)
      (aif (dotted-get-cell sym-or-dotted-get)
           (safe-set-cell-value it value)
           (runtime-error "~a: No such variable" op-str))
      (aif (env-cell *current-env* sym-or-dotted-get)
           (safe-set-cell-value it value)
           (runtime-error "~a: No such variable" op-str))))


