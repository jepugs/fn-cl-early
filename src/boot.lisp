;;;; boot.lisp -- interpreter bootstrapping

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

(defpackage :fn.boot
  (:documentation "interpreter bootstrapping")
  (:use :cl :fn.util :fn.values :fn.code :fn.eval))

(in-package :fn.boot)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro with-syms (names &body body)
  `(let ,(mapcar $`(,(intern (concatenate 'string (string-upcase $) "-SYM"))
                     (fnintern ,$))
                 names)
     ,@body))

(defmacro fn-defmethod (name dispatch-params params &body impls)
  (with-gensyms (method dispatch-params-var params-var)
    `(let* ((,dispatch-params-var (list ,@dispatch-params))
            (,params-var ,params)
            (,method (make-fnmethod :params ,params-var
                                    :dispatch-params ,dispatch-params-var)))
       (safe-add-global (fnintern ,name) ,method nil "bootstrap: ")
       ,@(mapcar $`(set-impl ,method
                             (mapcar #'fn.eval::get-var (list ,@(car $)))
                             (make-fnfun :body ,(cadr $)))
                 impls))))

(defun error-constructor (args)
  (declare (ignore args))
  (runtime-error "Can't construct this type"))

(defmacro fn-defclass (name &key (fields 'nil) (constructor '#'error-constructor))
  (with-gensyms (sym-var)
    `(let ((,sym-var (fnintern ,name)))
       (safe-add-global ,sym-var
                        (make-fnclass :name ,sym-var
                                      :fields (list ,@fields)
                                      :constructor (make-fnfun :body ,constructor))))))

(defmacro fn-defun (name &body body)
  `(safe-add-global (fnintern ,name)
                    (make-fnfun :body (lambda (args) ,@body))))

(defun bool (x)
  (if x true false))

(defun init-interpreter-hook ()
  (with-syms ("x" "seq" "obj" "left" "right")
    ;; built-in classes
    (fn-defclass "Bool")
    (fn-defclass "Class" :fields (name-sym constructor-sym))
    (fn-defclass "Function")
    (fn-defclass "List" :constructor $(apply #'fnlist $))
    (fn-defclass "Module")
    (fn-defclass "Method")
    (fn-defclass "Null")
    (fn-defclass "Num")
    (fn-defclass "String" :constructor $(apply #'fnstring $))

    ;; sequence methods
    (fn-defmethod "head" (seq-sym) (make-param-list :pos `((,seq-sym)))
      ((list-class-sym) $(if (eq (car $) empty)
                             fnnull
                             (caar $)))
      ((string-class-sym) $(if (= (length (car $)) 0)
                               fnnull
                               (string (aref (car $) 0)))))
    (fn-defmethod "tail" (seq-sym) (make-param-list :pos `((,seq-sym)))
      ((list-class-sym) $(if (eq (car $) empty)
                             empty
                             (cdar $)))
      ((string-class-sym) $(if (= (length (car $)) 0)
                               ""
                               (subseq (car $) 1))))

    (fn-defmethod "cons" (seq-sym) (make-param-list :pos `((,x-sym) (,seq-sym)))
      ((list-class-sym) $(cons (car $) (cadr $)))
      ((string-class-sym) $(if (and (string? (car $))
                                    (= (length (car $)) 1))
                               (concatenate 'string (car $) (cadr $))
                               (runtime-error
                                "(cons String): first argument not a length-1 string"))))

    (fn-defmethod "equal-method" (obj-sym) (make-param-list :pos `((,obj-sym) (,x-sym))))
    (fn-defmethod "add-method" (left-sym right-sym)
        (make-param-list :pos `((,left-sym) (,right-sym))))

    (fn-defun "gensym"
      (unless (length= args 0)
        (runtime-error "too many arguments for gensym"))
      (fngensym))

    (fn-defun "+"
      (when (length< args 1)
        (runtime-error "too few arguments for +"))
      (unless (every #'num? args)
        (runtime-error "Arguments to + must be numbers"))
      (num (apply #'+ args)))
    (fn-defun "-"
      (when (length< args 1)
        (runtime-error "too few arguments for -"))
      (unless (every #'num? args)
        (runtime-error "Arguments to - must be numbers"))
      (num (apply #'- args)))
    (fn-defun "/"
      (when (length< args 1)
        (runtime-error "too few arguments for /"))
      (unless (every #'num? args)
        (runtime-error "Arguments to / must be numbers"))
      (num (apply #'/ args)))
    (fn-defun "*"
      (when (length< args 1)
        (runtime-error "too few arguments for *"))
      (unless (every #'num? args)
        (runtime-error "Arguments to * must be numbers"))
      (num (apply #'* args)))
    (fn-defun "pow"
      (unless (length= args 2)
        (runtime-error "wrong number of arguments for *"))
      (unless (every #'num? args)
        (runtime-error "Arguments to pow must be numbers"))
      (num (expt (car args) (cadr args))))

    (fn-defun ">"
      (unless (length>= args 2)
        (runtime-error "too few arguments for >"))
      (bool (apply #'> args)))
    (fn-defun "<"
      (unless (length>= args 2)
        (runtime-error "too few arguments for <"))
      (bool (apply #'< args)))
    (fn-defun ">="
      (unless (length>= args 2)
        (runtime-error "too few arguments for >="))
      (bool (apply #'>= args)))
    (fn-defun "<="
      (unless (length>= args 2)
        (runtime-error "too few arguments for <="))
      (bool (apply #'<= args)))

    (fn-defun "="
      (unless (length= args 2)
        (runtime-error "wrong number of arguments for ="))
      (if (equalp (car args) (cadr args))
          true
          false))

    (fn-defun "print"
      (if (length= args 1)
          (fnprint (car args))
          (runtime-error "wrong number of arguments for print"))
      fnnull)
    (fn-defun "println"
      (if (length= args 1)
          (fnprintln (car args))
          (runtime-error "wrong number of arguments for print"))
      fnnull)
    (fn-defun "load"
      (unless (length= args 1)
        (runtime-error "wrong number of arguments for load"))
      (unless (stringp (car args))
        (runtime-error "load: argument must be a string"))
      (with-open-file (in (car args) :direction :input)
        (let ((ast (fn.parser:parse (fn.scanner:scan in (car args)))))
          (mapc $(handler-case (eval-ast $)
                   (fn-error (x)
                     (format t "Error encountered during load: ~a~%" x)))
                ast)))
      fnnull)
    (fn-defun "runtime-error"
      (runtime-error "~a" (show (car args))))
    (fn-defun "exit"
      (funcall #'sb-ext:exit
               :code (if args (floor (car args)) 0)))))


(eval-when (:load-toplevel :execute)
  (setf *interpreter-initialization-hook* #'init-interpreter-hook))
