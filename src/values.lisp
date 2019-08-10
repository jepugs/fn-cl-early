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
   :empty :empty? :fnnull :fnnull? :true :true? :false :false? :num :num?
   ;; symbols
   :sym :sym? :sym-id :sym-name :sym-name-is?
   :symtab :by-name :by-id :symtab-intern
   ;; sequences
   :fnlist :fnlist? :fnlist->list :fnappend :fnmapcar :fnmapcan :fnstring :string? :table :table?
   ;; functions and local environments
   :cell :make-cell :cell-value :value :cell-mutable :mutable
   :init-env :env-get :env-add
   :param-list :make-param-list :pos :keyword :vari
   :fnfun :params :body :env :closure :param-list-syms :make-fnfun :fnfun?
   ;; general objects
   :fntype :name :fields :fntype-name :make-fntype :fntype?
   :fnobj :type :contents :make-fnobj :fnobj?
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
  (defun num? (x) (typep x 'double-float)))

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
  ((next-id :initform 0)
   (by-name :initform (make-hash-table :test 'equal))
   (by-id :initform (make-array 256 :fill-pointer 0))))
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

;;; environments are hash tables mapping symbol IDs onto cells.

(defun init-env (syms &optional parent)
  "Create a new environment with uninitialized variables corresponding to the provided symbols."
  (let ((res (if parent
                 (copy-ht parent)
                 (make-hash-table :test #'eql :size (length syms)))))
    (mapc $(setf (gethash (sym-id $) res)
                 (make-cell :mutable t))
          syms)
    res))

(defun env-get (env sym)
  "Get a value cell from the environment. Returns NIL if no cell exists."
  (gethash (sym-id sym) env))

(defun env-add (env sym cell)
  "Add a new cell to an environment"
  (setf (gethash (sym-id sym) env) cell))

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
  (constructor nil :read-only t))

(defstruct (fnobj (:predicate fnobj?) :copier)
  (class nil :type fnclass :read-only t)
  ;; contents are stored in a hash table. Keys are symbol IDs
  (contents nil :type hash-table :read-only t))

(defstruct (fnmethod (:predicate fnmethod?) :copier)
  (params)
  ;; list of symbols indicating method names
  (dispatch nil :type hash-table :read-only t))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; pretty printing

(defun ->code-string (x)
  "Print objects as expressions"
  (cond
    ((empty? x) "[]")
    ((fnnull? x) "null")
    ((true? x) "true")
    ((false? x) "false")
    ((num? x) (princ-to-string x))
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
    ((num? x) (princ-to-string x))
    ((fnlist? x)
     (format nil "[~{~a~^ ~}]" (fnmapcar #'->string x)))
    ((string? x) x)
    ((sym? x) (slot-value x 'name))
    ((fnfun? x) "#<FUNCTION>")))

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
