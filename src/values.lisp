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
   :symtab :make-symtab :fnintern
   ;; sequences
   :fnlist :fnlist? :fnmapcar :fnmapcan :fnstring :string? :table :table?
   ;; functions and local environments
   :cell :make-cell :cell-value :value
   :init-env :env-get
   :fnfun :param-list :body :env :param-list-syms :fnfun
   ;; general objects
   :fntype :name :fields :fntype-name :make-fntype :fntype?
   :fnobj :type :contents :make-fnobj :fnobj?
   ;; pretty printing
   :->string :fnprint
   ;; quoting
   :bracket-sym-name :dollar-sym-name :dot-sym-name :quot-sym-name
   :ast->fnval))

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
      ;; are signed 62 bit numbers so this gives us spare
      :type (unsigned-integer 48)
      :read-only t))

(defun sym-name-is? (name sym)
  (string-equal (sym-name sym) name))

(defclass symtab ()
  ((next-id :initform 0)
   (by-name :initform (make-hash-table :test 'equal))
   (by-id :initform (make-array 256 :fill-pointer 0))))
(defun fnintern (name symtab)
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
(defun fnmapcar (fun x)
  (loop
     for src = x then (cdr x)
     until (empty? x)
     collect (funcall fun (car x))))
(defun fnmapcan (fun x)
  (loop
     for src = x then (cdr x)
     until (empty? x)
     nconc (funcall fun (car x))))

(defun fnstring (&rest objects)
  (apply #'concatenate 'string (mapcar #'->string objects)))
(defun string? (x) (stringp x))

(deftype table () 'hash-table)
(defun table? (x) (typep x table))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; functions and local environments

;;; a generic memory cell. This wraps an fn value so that modifications to the value in the cell
;;; will be seen across all closures, etc containing it
(defstruct cell
  value)

;;; environments are hash tables mapping symbol IDs onto cells.

(defun init-env (syms &optional parent)
  "Create a new environment with uninitialized variables corresponding to the provided symbols."
  (let ((res (if parent
                 (copy-ht parent)
                 (make-hash-table :test #'= :size (length syms)))))
    (mapc $(setf (gethash (sym-id $) vars) (make-cell)) syms)
    res))

(defun env-get (env sym)
  "Get a value cell from the environment. Returns NIL if no cell exists."
  (gethash (sym-id sym) env))

(defstruct fnfun
  ;; parameter list used to bind arguments
  (param-list nil :type list :read-only t)
  ;; either a list of AST expressions containing function code or a Common Lisp function object. In
  ;; the latter case, the function is invoked directly with a list of fn arguments, allowing Common
  ;; Lisp functionality to be provided to fn.
  (body nil :type (or list function) :read-only t)
  ;; environment including enclosed variables and arguments
  (env nil :type env :read-only t))

(defun param-list-syms (param-list)
  "Get a list of symbols bound by a parameter list. This function assumes that the param list has
 correct syntax."
  (reduce (lambda (acc x)
            (cond
              ;; add any plain symbol except & and _
              ((sym? x)
               (if (or (sym-name-is? "&" x)
                       (sym-name-is? "_" x))
                   acc
                   (cons x acc)))
              ;; from here we can assume the parameter is a list

              ;; required keywords
              ((and (sym? (car x))
                    (sym-name-is? quot-sym-name (car x)))
               (cons (cadr x) acc))

              ;; add optional args
              ((sym? (car x))
               (cons (car x) acc))

              ;; add required keywords
              (t (cons (cadar x) acc))))
          param-list
          :initial-value nil))

(defun fnfun (param-list body &optional parent-env)
  "Create a function object"
  (let ((params (param-list-syms param-list)))
    (make-fnfun :param-list param-list
                :body body
                ;; no reason to create an environment in this case
                :env (if (functionp body)
                         nil
                         (init-env params parent-env)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; general objects

(defstruct (fntype (:predicate fntype?) :copier)
  (name nil :type sym :read-only t)
  (fields nil :type list :read-only t))

(defstruct (fnobj (:predicate fnobj?) :copier)
  (type nil :type fntype :read-only t)
  ;; contents are stored as a plist
  (contents nil :type list))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; pretty printing

(defun ->string (x)
  "Convert an fn object to a string."
  (cond
    ((empty? x) "()")
    ((fnnull? x) "Null")
    ((true? x) "True")
    ((false? x) "False")
    ((num? x) (princ-to-string (slot-value x 'value)))
    ((fnlist? x)
     (format nil "(~{~a~^ ~})" (fnmapcar #'->string x)))
    ((string? x) (concatenate 'string "\"" x "\""))
    ((sym? x) (slot-value x 'name))
    ((fnobj? x) (format nil
                        "(~a ~{~a~^ ~})"
                        (type-name (fnobj-type x))
                        (mapcar #'->string (fnobj-contents x))))))

(defun fnprint (x &optional (stream *standard-output*))
  (princ (->string x) stream))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; quoting (i.e. converting ast to fn values)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant-1 bracket-sym-name "List")
  (defconstant-1 dollar-sym-name "dollar-fn")
  (defconstant-1 dot-sym-name "get")
  (defconstant-1 quot-sym-name "quote"))

(defun ast->fnval (a symtab)
  "Convert an ast to its fn value representation. This mutates the symbol table if necessary."
  (cond
    ((ast-string? a) (fnstring (slot-value a 'value)))

    ((ast-number? a) (num (slot-value a 'value)))

    ((ast-paren? a) (apply #'fnlist
                           (mapcar $(ast->fnval $ symtab)
                                   (slot-value a 'contents))))

    ((ast-bracket? a) (apply #'fnlist
                             ;; prepend the List symbol
                             (fnintern bracket-sym-name symtab)
                             (mapcar $(ast->fnval $ symtab)
                                     (slot-value a 'contents))))

    ;; FIXME: either implement these or emit proper errors
    ((ast-brace? a)
     (error "Brace syntax is not implemented"))

    ((ast-quot? a) (fnlist (fnintern quot-sym-name symtab)
                           (ast->fnval (slot-value a 'expr) symtab)))

    ((or (ast-quasiquot? a)
         (ast-unquot? a)
         (ast-unquot-splice? a))
     (error "Quasiquotation is not implemented"))

    ((ast-dollar? a) (fnlist (fnintern dollar-sym-name symtab)
                             (ast->fnval (slot-value a 'expr) symtab)))

    ((ast-dot? a) (fnlist (fnintern dot-sym-name symtab)
                          (ast->fnval (slot-value a 'left) symtab)
                          (ast->fnval (slot-value a 'right) symtab)))

    ((ast-sym? a) (fnintern (slot-value a 'name)))))
