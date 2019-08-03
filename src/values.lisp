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
  (:documentation "fn value representation for the tree-walking interpreter")
  (:use :cl :fn.util :fn.ast)
  (:export :empty :empty? :fnnull :fnnull? :true :true? :false :false? :num :num?))

(in-package :fn.values)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; constants


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro defconstant-1 (name value)
    "Like DEFCONSTANT but will not redefine a value on multiple successive calls."
    `(defconstant ,name (if (boundp ',name) ,name ,value)))

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

(deftype table () 'hash-table)
(defun table? (x) (typep x table))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; general objects

(defstruct fntype
  (name nil :type sym :read-only t)
  (fields nil :type list :read-only t))

(defstruct fnobj
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
