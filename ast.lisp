;;;; ast.lisp -- AST data structures

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

(defpackage :fn.ast
  (:documentation "AST data structures")
  (:use :cl :fn.util :fn.scanner)
  (:import-from :fn.scanner :line)
  (:export :line :value :contents :expr :name :contents :ast-string :ast-number :ast-paren
           :ast-bracket :ast-brace :ast-quot :ast-quasiquot :ast-unquot :ast-unquot-splice
           :ast-dollar :ast-dot :ast-sym :convert-ast))

(in-package :fn.ast)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; AST data structure generators

(defmacro def-ast (&body specs)
  "Defines AST data structures. Each spec is a list of symbols (NAME FIELDS ...). For each spec, a
 new structure is created with the provided field names. The name of the structure is prefixed with
 AST-. The constructor is also modified to not use keyword arguments and to share a name with the
 structure."
  `(progn
     ;;
     ,@(mapcar
        $(let ((name (intern
                      (concatenate 'string "AST-" (string (car $))))))
           `(defstruct (,name (:constructor ,name ,(cdr $)))
              ,@(cdr $)))
        specs)))


(def-ast
  (string line value)
  (number line value)
  (paren line contents)
  (bracket line contents)
  (brace line contents)
  (quot line expr)
  (quasiquot line expr)
  (unquot line expr)
  (unquot-splice line expr)
  (dollar line expr)
  (dot line left right)
  (sym line name))

(defgeneric convert-ast (object))

(defmethod convert-ast ((object list))
  (mapcar #'convert-ast object))

(defmethod convert-ast ((object t))
  object)

(defun convert-ast-helper (object &rest slots)
  (let ((name (subseq (symbol-name (class-name (class-of object))) 4)))
    (cons (intern name)
          (mapcar $(convert-ast (slot-value object $)) slots))))

(defmacro def-ast-converter (type &rest slots)
  `(defmethod convert-ast ((object ,type))
     (convert-ast-helper object ,@(mapcar $`',$ slots))))

(def-ast-converter ast-string value)
(def-ast-converter ast-number value)
(def-ast-converter ast-paren line contents)
(def-ast-converter ast-bracket line contents)
(def-ast-converter ast-brace line contents)
(def-ast-converter ast-quot expr)
(def-ast-converter ast-quasiquot expr)
(def-ast-converter ast-unquot expr)
(def-ast-converter ast-unquot-splice expr)
(def-ast-converter ast-dollar expr)
(def-ast-converter ast-dot left right)
(def-ast-converter ast-sym name)
