;;;; values.lisp -- fn value representation for the tree-walking evaluator

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

(defpackage :fn.values
  (:documentation "internal representations of values for the tree-walking interpreter")
  (:use :cl :fn.util :fn.ast)
  (:export
   ;; simple constants
   :empty :empty? :fnnull :fnnull? :true :true? :false :false? :num :num? :num-int?
   ;; symbols
   :sym :sym? :sym-id :sym-name :sym-name-is?
   :symtab :by-name :by-id :symtab-intern :make-symtab
   ;; operator symbols
   :apply-sym :case-sym :cond-sym :class-of-sym :def-sym :defclass-sym :defmacro-sym :defmethod-sym
   :defvar-sym :do-sym :dollar-fn-sym :fn-sym :get-field-sym :get-sym :if-sym :import-sym :let-sym
   :quote-sym :quasiquote-sym :set-sym :unquote-sym :unquote-splice-sym
   ;; default class symbols
   :bool-class-sym :class-class-sym :function-class-sym :list-class-sym :module-class-sym
   :method-class-sym :null-class-sym :num-class-sym :string-class-sym
   ;; built-in class fields
   :hd-sym :tl-sym :name-sym :fields-sym :constructor-sym :vars-sym :macros-sym
   ;; built-in methods
   :call-sym :get-method-sym :set-method-sym :add-method-sym :mul-method-sym :sub-method-sym
   ;; other built-in symbols
   :amp-sym :hash-sym :wildcard-sym
   ;; sequences
   :fnlist :fnlist? :fnlist->list :fnlist-length :fnappend :fnmapcar :fnmapcan
   :fnstring :string? :table :table?
   ;; modules and environments
   :cell :make-cell :cell-value :value :cell-mutable :mutable
   :make-env :add-global-cell :add-cell :get-module-cell :get-cell :get-macro :set-global-macro
   :env-module :env-table
   :fnmodule :name :vars :macros :make-fnmodule :fnmodule-name :fnmodule-vars :fnmodule-macros
   :add-module-cell :fnmodule?
   ;; functions
   :param-list :make-param-list :pos :keyword :vari
   :fnfun :params :body :env :closure :param-list-syms :make-fnfun :fnfun?
   ;; general objects
   :fnclass :fields :constructor :fnclass-name :make-fnclass :fnclass-fields :fnclass-constructor :fnclass?
   :fnobj :class :contents :make-fnobj :fnobj-class :fnobj-contents :fnobj?
   :fnmethod :make-fnmethod :fnmethod?
   ;; pretty printing
   :->string :fnprint :fnprintln :->code-string :fnprint-code :fnprintln-code
   ;; quoting
   ;;:bracket-sym-name :dollar-sym-name :dot-sym-name :quot-sym-name
   :ast->fnval :fnval->ast))

(in-package :fn.values)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; constants

(eval-when (:compile-toplevel :load-toplevel :execute)

  (defclass empty () ()
    (:documentation "empty list"))
  (defconstant-1 empty (make-instance 'empty))
  (defun empty? (x) (eq x empty))

  (defclass fnnull () ()
    (:documentation "null value"))
  (defconstant-1 fnnull  (make-instance 'fnnull))
  (defun fnnull? (x) (eq x fnnull))

  (defclass true () ()
    (:documentation "true boolean"))
  (defconstant-1 true (make-instance 'true))
  (defun true? (x) (eq x true))

  (defclass false () ()
    (:documentation "false boolean"))
  (defconstant-1 false (make-instance 'false))
  (defun false? (x) (eq x false))

  (deftype num () 'double-float)
  (defun num (x) (coerce x 'double-float))
  (defun num? (x) (typep x 'double-float))
  (defun num-int? (x)
    (= (truncate x) x)))

(defstruct (sym (:copier nil)
                (:predicate sym?))
  (name nil :type string :read-only t)
  ;;Unique integer identifier for this symbol
  (id nil
      ;; I use 48-bits here to ensure that an immediate representation will be used. FIXNUMs in SBCL
      ;; are signed 62 bit numbers so this gives us spare bits
      :type (unsigned-byte 48)
      :read-only t))

(defun sym-name-is? (name sym)
  (string-equal (sym-name sym) name))

(defclass symtab ()
  ((next-id :initarg :next-id
            :initform 0)
   (by-name :initarg :by-name
            :initform (make-hash-table :test 'equal))
   (by-id :initarg :by-id
          :initform (make-array 256 :fill-pointer 0))))
(defun symtab-intern (name symtab)
  "Get a symbol from the symbol table. If the symbol does not exist, it is created and added to the
 table."
  (with-slots (next-id by-name by-id) symtab
    (aif (gethash name by-name)
         it
         (let ((s (make-sym :name name :id next-id)))
           (setf (gethash name by-name) s)
           (vector-push-extend s by-id)
           (incf (slot-value symtab 'next-id))
           s))))

;;; constant symbols will be interned in every instance of fn

(defparameter template-symtab (make-instance 'symtab))

(macrolet ((def-sym-var (name)
             `(defparameter ,(intern (concatenate 'string (string-upcase name) "-SYM"))
                (symtab-intern ,name template-symtab)))
           (def-syms (&body lst)
             `(progn ,@(mapcar $`(def-sym-var ,$) lst))))
  ;; special operators
  (def-syms "apply" "case" "cond" "class-of" "def" "defclass" "defmacro" "defmethod"
            "defvar" "do" "dollar-fn" "fn" "get-field" "get" "if" "import" "let" "quote"
            "quasiquote" "set" "unquote" "unquote-splice")
  ;; misc special syms
  (defparameter wildcard-sym (symtab-intern "_" template-symtab))
  (defparameter hash-sym (symtab-intern "#" template-symtab))
  (defparameter amp-sym (symtab-intern "&" template-symtab))

  ;; built-in class fields
  (def-syms "hd" "tl" "name" "fields" "constructor" "vars" "macros")

  ;; built-in class names
  (defparameter bool-class-sym (symtab-intern "Bool" template-symtab))
  (defparameter class-class-sym (symtab-intern "Class" template-symtab))
  (defparameter function-class-sym (symtab-intern "Function" template-symtab))
  (defparameter list-class-sym (symtab-intern "List" template-symtab))
  (defparameter module-class-sym (symtab-intern "Module" template-symtab))
  (defparameter method-class-sym (symtab-intern "Method" template-symtab))
  (defparameter null-class-sym (symtab-intern "Null" template-symtab))
  (defparameter num-class-sym (symtab-intern "Num" template-symtab))
  (defparameter string-class-sym (symtab-intern "String" template-symtab))

  ;; built-in methods
  (def-syms "call" "get-method" "set-method" "add-method" "mul-method" "sub-method"
            "div-method"))

(defun make-symtab ()
  (with-slots (next-id by-name by-id) template-symtab 
    (make-instance 'symtab
                   :next-id next-id
                   :by-name (copy-ht by-name)
                   :by-id (make-array (length by-id)
                                      :initial-contents by-id
                                      :fill-pointer (fill-pointer by-id)
                                      :adjustable t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; sequences/collections

(defun fnlist (&rest objects)
  "Make an fn style list"
  (if objects
      (do* ((res (cons (car objects) empty))
            (tl res (cdr tl))
            (src (cdr objects) (cdr src)))
           ((null src) res)
        (rplacd tl (cons (car src) empty)))
      empty))
(defun fnlist? (x)
  (or (empty? x) (consp x)))
(defun fnlist->list (x)
  (let ((res (copy-list x)))
    (rplacd (last res) nil)
    res))
(defun fnlist-length (x)
  (rlambda (acc lst) (0 x)
    (if (empty? lst)
        acc
        (recur (+ 1 acc) (cdr x)))))
(defun fnappend (lst0 lst1)
  (if (empty? lst0)
      lst1
      (let ((res (copy-list lst0)))
        (rplacd (last res) lst1)
        res)))
(defun fnmapcar (fun x)
  (loop
     for src = x then (cdr src)
     until (empty? src)
     collect (funcall fun (car src))))
(defun fnmapcan (fun x)
  (loop
     for src = x then (cdr src)
     until (empty? src)
     nconc (funcall fun (car src))))

(defun fnstring (&rest objects)
  (apply #'concatenate 'string
         (mapcar $(if (string? $)
                      $
                      (->string $))
                 objects)))
(defun string? (x) (stringp x))

(deftype table () 'hash-table)
(defun table? (x) (typep x 'table))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; environments, functions, and objects

;;; a generic memory cell. This wraps an fn value so that modifications to the value in the cell
;;; will be seen across all closures, etc containing it
(defstruct cell
  value
  (mutable nil :read-only t))

(defstruct env
  (table (make-hash-table :test 'eql))
  module
  (parent nil))

;;; environments are hash tables mapping symbol IDs onto cells.
(defstruct (fnmodule (:predicate fnmodule?) :copier)
  (name nil :type sym)
  (vars (make-hash-table) :type hash-table)
  (macros (make-hash-table) :type hash-table))

;; module/environment functions
(defun add-global-cell (env sym cell)
  "Add a new lexical cell to an environment"
  (setf (gethash (sym-id sym) (fnmodule-vars (env-module env)))
        cell))

(defun add-module-cell (mod sym cell)
  "Add a new cell to a module"
  (setf (gethash (sym-id sym) (fnmodule-vars mod))
        cell))

(defun add-cell (env sym cell)
  "Add a new cell to a module given"
  (setf (gethash (sym-id sym) env) cell))

(defun get-module-cell (mod sym)
  "Get the CELL, if any, associated with SYM in the provided lexical environment and module. Returns
 NIL on failure."
  (gethash (sym-id sym) (fnmodule-vars mod)))

(defun get-cell (env sym)
  "Get the CELL, if any, associated with SYM in the provided lexical environment and module. Returns
 NIL on failure."
  (or (gethash (sym-id sym) (env-table env))
      (aif (env-parent env)
           (get-cell it sym)
           (gethash (sym-id sym) (fnmodule-vars (env-module env))))))

(defun get-macro (env sym)
  "Get the macro associated with SYM in the provided lexical environment and module."
  (gethash (sym-id sym) (fnmodule-macros (env-module env))))

(defun set-global-macro (env sym value)
  "Set the macro associated with SYM in the provided and module."
  (setf (gethash (sym-id sym) (fnmodule-macros (env-module env))) value))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; functions

;; function parameter object. Used to efficiently bind arguments.
(defstruct (param-list (:predicate fnparams?))
  ;; ALIST of positional parameters. Non-null value indicates that the parameter is optional.
  (pos nil :type list :read-only t)
  ;; ALIST of keyword parameters
  (keyword nil :type list :read-only t)
  ;; variadic parameter
  (vari nil :type (or sym null) :read-only t))

(defstruct (fnfun (:predicate fnfun?))
  ;; parameter list
  (params nil :type (or param-list null) :read-only t)
  ;; either a list of fn code objects (see eval.lisp) or a function. In the latter case, the
  ;; function is invoked directly with a list of fn values, allowing definition of built-in
  ;; functions. In the former, the code in the body is evaluated in an appropriate lexical
  ;; environment.
  (body nil :type (or list function) :read-only t)
  ;; captured lexical environment
  (closure nil :read-only t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; object system

(defstruct (fnclass (:predicate fnclass?) :copier)
  (name nil :type sym :read-only t)
  (fields nil :type list :read-only t)
  (constructor nil))

(defstruct (fnobj (:predicate fnobj?) :copier)
  (class nil :type fnclass :read-only t)
  ;; contents are stored in a hash table. Keys are symbol IDs
  (contents nil :type hash-table :read-only t))

(defstruct (fnmethod (:predicate fnmethod?) :copier)
  (params nil :type param-list :read-only t)
  (dispatch-params nil :type list :read-only t)
  (impls nil :type hash-table :read-only t)
  (default-impl nil))

(defun get-impl (m types)
  (gethash types (fnmethod-impls m)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; pretty printing

(defun ->code-string (x)
  "Print objects as expressions"
  (cond
    ((empty? x) "[]")
    ((fnnull? x) "null")
    ((true? x) "true")
    ((false? x) "false")
    ((num? x) (format nil "~f" x))
    ((fnlist? x)
     (format nil "[~{~a~^ ~}]" (fnmapcar #'->code-string x)))
    ((string? x) (concatenate 'string "\"" x "\""))
    ((sym? x) (concatenate 'string
                           "'"
                           (slot-value x 'name)))
    ((fnfun? x) "#<FUNCTION>")))

(defun ->string (x)
  "Convert an fn object to a string."
  (cond
    ((empty? x) "[]")
    ((fnnull? x) "null")
    ((true? x) "true")
    ((false? x) "false")
    ((num? x) (format nil "~f" x))
    ((fnlist? x)
     (format nil "[~{~a~^ ~}]" (fnmapcar #'->string x)))
    ((string? x) x)
    ((sym? x) (slot-value x 'name))
    ((fnfun? x) "#<FUNCTION>")
    ((fnclass? x) (concatenate 'string
                               "#<CLASS: "
                               (sym-name (fnclass-name x))
                               ">"))
    ((fnobj? x)
     (format nil
             "(~a ~{~a~^ ~})"
             (sym-name (fnclass-name (fnobj-class x)))
             (->> x
               (fnobj-contents)
               (ht->plist)
               (group 2)
               (reverse)
               (mapcar $(->string (cell-value (cadr $)))))))
    (t "#<UNRECOGNIZED-FOREIGN-OBJECT>")))

(defun fnprint (x &optional (stream *standard-output*))
  (princ (->string x) stream))
(defun fnprintln (x &optional (stream *standard-output*))
  (fnprint x stream)
  (terpri stream))

(defun fnprint-code (x &optional (stream *standard-output*))
  (princ (->code-string x) stream))
(defun fnprintln-code (x &optional (stream *standard-output*))
  (fnprint-code x stream)
  (terpri stream))
