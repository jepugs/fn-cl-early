;;;; code.lisp -- intermediate representation and syntax checking

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

(defpackage :fn.code
  (:documentation "intermediate representation and syntax checking")
  (:use :cl :fn.util :fn.ast :fn.values)
  (:export :code :origin :data :make-code :code-origin :code-data :code? :code-intern :ast->code
           :code-list? :code-sym? :code-sym-name :code-sym-id :code-car :code-cadr :code-cdr
           :code-literal? :code-quoted-sym? :code-op-is :code->fnvalue :validate-code
           :bracket-sym-name :brace-sym-name :dollar-sym-name :dot-sym-name :quot-sym-name
           :quasiquot-sym-name :unquot-sym-name :unquot-splice-sym-name :true-sym-name
           :false-sym-name :null-sym-name))

(in-package :fn.code)

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
(defconstant-1 dot-sym-name "get")
(defconstant-1 quot-sym-name "quote")
(defconstant-1 quasiquot-sym-name "quasiquote")
(defconstant-1 unquot-sym-name "unquote")
(defconstant-1 unquot-splice-sym-name "unquote-splice")
(defconstant-1 true-sym-name "true")
(defconstant-1 false-sym-name "false")
(defconstant-1 null-sym-name "null")

;;; Each Code object retains a reference to the AST object from which it was derived (for error
;;; handling).

(defstruct (code (:constructor make-code (origin data))
                 (:predicate code?))
  ;; this may be an AST object or a macro
  origin
  ;; the converted ast object. This may be either an fn value or a list.
  data)

(defun code-intern (origin name symtab)
  "Like SYMTAB-INTERN but wraps the generated symbol in a code object."
  (make-code origin (symtab-intern name symtab)))

(defun ast->code (a symtab)
  "Converts an AST object to code."
  (let ((o (slot-value a 'origin)))
    (make-code
     o
     (cond
       ((ast-string? a) (fnstring (slot-value a 'value)))
       ((ast-number? a) (num (slot-value a 'value)))

       ((ast-paren? a)  (mapcar $(ast->code $ symtab)
                                (slot-value a 'contents)))
       ((ast-bracket? a) (cons (code-intern o bracket-sym-name symtab)
                               (mapcar $(ast->code $ symtab)
                                       (slot-value a 'contents))))
       ((ast-brace? a) (cons (code-intern o brace-sym-name symtab)
                             (mapcar $(ast->code $ symtab)
                                     (slot-value a 'contents))))

       ((ast-quot? a) (list (code-intern o quot-sym-name symtab)
                            (ast->code (slot-value a 'expr) symtab)))
       ((ast-quasiquot? a) (list (code-intern o quasiquot-sym-name symtab)
                                 (ast->code (slot-value a 'expr) symtab)))
       ((ast-unquot? a) (list (code-intern o unquot-sym-name symtab)
                              (ast->code (slot-value a 'expr) symtab)))
       ((ast-unquot-splice? a) (list (code-intern o unquot-splice-sym-name symtab)
                                     (ast->code (slot-value a 'expr) symtab)))

       ((ast-dollar? a) (list (code-intern o dollar-sym-name symtab)
                              (ast->code (slot-value a 'expr) symtab)))

       ((ast-dot? a) (list (code-intern o dot-sym-name symtab)
                           (ast->code (slot-value a 'left) symtab)
                           (ast->code (slot-value a 'right) symtab)))

       ((ast-sym? a) (symtab-intern (slot-value a 'name) symtab))))))

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

(defun code-quoted-sym? (c)
  (with-slots (data) c
    (and (listp data)
         (length= data 2)
         (every #'code-sym? data)
         (string= quot-sym-name (code-sym-name (car data))))))

(defun code-op-is (c op-name)
  "Check whether C has a certain symbol in its operator position. Assumes C contains a list."
  (with-slots (data) c
    (and (code-sym? (car data))
         (string= (code-sym-name (car data)) op-name))))

(defun code->fnvalue (c)
  (with-slots (data) c
    (if (listp data)
        (apply #'fnlist (mapcar #'code->fnvalue data))
        data)))

(eval-when (:load-toplevel :compile-toplevel :execute)
  (defmacro code-error (c format-string &rest format-args)
    `(fn-error (slot-value ,c 'origin)
               ,format-string
               ,@format-args)))

(defun validate-code (c)
  "Ensures that all special forms have correct syntax. Emits an fn-error on malformed expressions."
  (cond
    ((code-literal? c) nil)
    ((code-sym? c) nil)
    ((code-list? c)
     (when (null (code-data c))
       (fn-error (code-origin c) "empty list"))
     (let ((op (code-car c))
           (args (code-cdr c))
           (o (slot-value c 'origin)))
       (if (code-sym? op)
           (let ((op-name (code-sym-name op)))
             (cond
               ((string= op-name "apply") (validate-apply o args))
               ((string= op-name "case") (validate-case o args))
               ((string= op-name "cond") (validate-cond o args))
               ((string= op-name "def") (validate-def o args))
               ((string= op-name "defclass") (validate-defclass o args))
               ((string= op-name "defmacro") (validate-defmacro o args))
               ((string= op-name "defmethod") (validate-defmethod o args))
               ((string= op-name "defvar") (validate-defvar o args))
               ((string= op-name "do") (validate-do o args))
               ((string= op-name "dollar-fn") (validate-dollar-fn o args))
               ((string= op-name "fn") (validate-fn o args))
               ((string= op-name "get") (validate-get o args))
               ((string= op-name "get-field") (validate-get-field o args))
               ((string= op-name "let") (validate-let o args))
               ((string= op-name "quote") (validate-quote o args))
               ((string= op-name "quasiquote") (validate-quasiquote o args))
               ((string= op-name "unquote") (validate-unquote o args))
               ((string= op-name "unquote-splice") (validate-unquote-splice o args))
               ((string= op-name "set") (validate-set o args))))
           (mapcar #'validate-code (code-data c)))))))

(defun check-arg-length= (o args n name)
  (unless (length= args n)
    (fn-error o "wrong number of arguments for ~a" name)))
(defun check-arg-length>= (o args n name)
  (when (length< args n)
    (fn-error o "too few arguments for ~a" name)))

(defun validate-exprs (args)
  (mapc #'validate-code args))

(defun validate-params (o data &optional (prefix ""))
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
                        (fn-error
                         o
                         "~a~a"
                         prefix
                         "required positionals parameters can't come after optional parameters"))
                      (when key
                        (fn-error
                         o
                         "~a~a"
                         prefix
                         "non-keyword parameters cannot follow keyword parameters"))
                      (check-params (cdr lst) nil nil))

                     ;; required keyword argument
                     ((code-quoted-sym? p)
                      (when opt
                        (fn-error
                         o
                         "~a~a"
                         prefix
                         "required keyword parameters cannot follow optional parameters"))
                      (check-params (cdr lst) t nil))

                     ;; optional arguments
                     ((and (code-list? p)
                           (length= (code-data p) 2))
                      (let ((name (code-car p)))
                        (if (code-quoted-sym? name)
                            (check-params (cdr lst) t opt)
                            (if key
                                (fn-error
                                 o
                                 "~a~a"
                                 prefix
                                 "non-keyword parameters cannot follow keyword parameters")
                                (check-params (cdr lst) nil t)))))

                     (t (fn-error o
                                  "~a~a"
                                  prefix
                                  "malformed parameter"))))))
           ;; check variadic parameter
           (check-vari-param (lst)
             (unless (length= lst 2)
               (fn-error o
                         "~a~a"
                         prefix
                         "too many parameters after &"))
             (unless (code-sym? (cadr lst))
               (fn-error o
                         "~a~a"
                         prefix
                         "malformed variadic parameter"))))
    (if (listp data)
        (check-params data nil nil)
        (fn-error o
                  "~a~a"
                  prefix
                  "parameters must form a list"))))


(defun validate-apply (o args)
  (check-arg-length>= o args 2 "apply")
  (validate-exprs args))

(defun validate-case (o args)
  (declare (ignorable args))
  (fn-error o "case is not implemented :("))

(defun validate-cond (o args)
  (check-arg-length>= o args 1 "cond")
  (unless (evenp (length args))
    (fn-error o "odd number of arguments to cond"))
  (validate-exprs args))

(defun validate-def (o args)
  (check-arg-length>= o args 2 "def")
  (let ((place (car args)))
    (cond
      ;; variable definition
      ((code-sym? place)
       (unless (length= args 2)
         (fn-error o "wrong number of arguments for definition of ~a" (code-sym-name place)))
       (validate-code (cadr args)))
      ;; function definition
      ((and (code-list? place)
            (code-sym? (code-car place)))
       (validate-params o
                        (code-cdr place)
                        "in function definition parameters: ")
       (validate-exprs (cdr args)))
      ;; method definition
      ((and (code-list? place)
            (code-list? (code-car place))
            (code-sym? (code-car (code-car place))))
       (validate-params o
                        (code-cdr place)
                        "in method definition parameters: ")
       (validate-exprs (cdr args)))
      (t (fn-error o
                   "invalid place in def form")))))

(defun validate-defclass (o args)
  (check-arg-length= o args 2 "defclass")
  (let ((name (car args))
        (params (cadr args)))
    (unless (code-sym? name)
      (fn-error o "invalid class name"))
    (validate-params o (code-data params) "in defclass parameters: ")))

(defun validate-defmacro (o args)
  (check-arg-length>= o args 2 "defmacro")
  (unless (and (code-list? (car args))
               (code-sym? (code-car (car args))))
    (fn-error o "malformed macro prototype"))
  (validate-params o (code-cdr (car args)) "in defmacro parameters: ")
  (validate-exprs (cdr args)))

(defun validate-defmethod (o args)
  (check-arg-length= o args 1 "defmethod")
  (let ((proto (car args)))
   (unless (and (code-list? proto)
                (code-list? (code-car proto))
                (code-sym? (code-car (code-car proto))))
     (fn-error o "malformed defmethod expression"))
   (validate-params o (code-cdr proto))))

(defun validate-defvar (o args)
  (check-arg-length= o args 2 "defvar")
  (let ((place (car args)))
    (unless (code-sym? place)
      (fn-error o "invalid place in def form")))
  (validate-code (cadr args)))

(defun validate-do (o args)
  (check-arg-length>= o args 1 "do")
  (validate-exprs args))

(defun validate-dollar-fn (o args)
  (check-arg-length= o args 1 "dollar-fn")
  (validate-exprs args))

(defun validate-fn (o args)
  (check-arg-length>= o args 2 "fn")
  (validate-params o (code-data (car args)) "in fn parameters: ")
  (validate-exprs (cdr args)))

(defun validate-get (o args)
  (declare (ignore o args)))

(defun validate-get-field (o args)
  (declare (ignore o args)))

(defun validate-let-bindings (o x)
  (unless (code-list? x)
    (fn-error o "let bindings must be a list"))
  (unless (evenp (length (code-data x)))
    (fn-error o "odd number of items in let binding list"))
  (let ((pairs (group 2 (code-data x))))
    (unless (every $(code-sym? (car $)) pairs)
      (fn-error o "let vars must be symbols"))
    (mapc $(validate-code (cadr $)) pairs)))

(defun validate-let (o args)
  (check-arg-length>= o args 2 "let")
  (validate-let-bindings o (car args))
  (validate-exprs (cdr args)))

;; quote doesn't do validation
(defun validate-quote (o args)
  (check-arg-length= o args 1 "quote"))

(defun validate-qq-expr (o expr level)
  (if (and (code-list? expr)
           (not (null (code-data expr))))
      (if (code-sym? (code-car expr))
          (let ((op (code-sym-name (code-car expr))))
            (cond
              ((and (or (string= op unquot-sym-name)
                        (string= op unquot-splice-sym-name))
                    (length= (code-data expr) 2))
               (if (= level 1)
                   (validate-code (code-cadr expr))
                   (validate-qq-expr o (code-cadr expr) (- level 1))))
              ((and (string= op quasiquot-sym-name)
                    (length= (code-data expr) 2))
               (validate-qq-expr o (code-cadr expr) (+ level 1)))
              (t (mapc $(validate-qq-expr o $ level) (code-data expr)))))
          (mapc $(validate-qq-expr o $ level) (code-data expr)))
      ;; all atoms are automatically valid
      nil))

(defun validate-quasiquote (o args)
  (check-arg-length= o args 1 "quasiquote")
  (validate-qq-expr o (car args) 1))

(defun validate-unquote (o args)
  (declare (ignore args))
  (fn-error o "unquote outside of quasiquote"))

(defun validate-unquote-splice (o args)
  (declare (ignore args))
  (fn-error o "unquote-splice outside of quasiquote"))

(defun validate-set (o args)
  (declare (ignore o args)))
