;;;; boot.lisp -- runtime bootstrapping

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
  (:documentation "runtime bootstrapping")
  (:use :cl :fn.util :fn.values :fn.code :fn.runtime :fn.eval)
  (:export :init-runtime))

(in-package :fn.boot)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro with-syms (names &body body)
  `(let ,(mapcar $`(,(intern (concatenate 'string (string-upcase $) "-SYM"))
                     (fnintern ,$))
                 names)
     ,@body))

(defmacro fn-defmethod (name dispatch-params params &body impls)
  "Make an fn method definition. IMPLS is a list of implementation specifiers, consisting of a
 parenthesized list of class symbols and a function body. The variable METHOD is bound to refer to
 the created method and ARGS is the name of the function argument list. Default method may be
 specified by providing () as arguments."
  (with-gensyms (dispatch-params-var params-var)
    `(let* ((,dispatch-params-var (list ,@dispatch-params))
            (,params-var ,params)
            (method (make-fnmethod :name ,name
                                   :params ,params-var
                                   :dispatch-params ,dispatch-params-var)))
       (safe-add-global (fnintern ,name) method nil "during bootstrapping: ")
       ,@(mapcar $(if (null (car $))
                      `(setf (slot-value method 'default-impl)
                             (make-fnfun :body
                                         (lambda (args)
                                           ;; used to check parameters
                                           (fn.eval::params-alist ,params-var
                                                                  args
                                                                  *current-env*
                                                                  ,name)
                                           ,@(cdr $))))
                      `(set-impl method
                                 (mapcar $(env-var *current-env* $) (list ,@(car $)))
                                 (make-fnfun :body
                                             (lambda (args)
                                               ;; used to check parameters
                                               (fn.eval::params-alist ,params-var
                                                                      args
                                                                      *current-env*
                                                                      ,name)
                                               ,@(cdr $)))))
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

(defun check-arglen (fun-name args op num)
  (case op
    ((>=)
     (when (length< args num)
       (runtime-error "~a: Too few arguments" fun-name)))
    ((=)
     (unless (length= args num)
       (runtime-error "~a: Wrong number of arguments" fun-name)))))

(defmacro fn-defun (name (args-op args-num) &body body)
  `(safe-add-global (fnintern ,name)
                    (make-fnfun :body (lambda (args)
                                        (check-arglen ,name args ',args-op ,args-num)
                                        ,@body))))

(defun bool (x)
  (if x true false))

(defun init-runtime-hook ()
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
      ((list-class-sym) (if (eq (car args) empty)
                            fnnull
                            (caar args)))
      ((string-class-sym) (if (= (length (car args)) 0)
                              fnnull
                              (string (aref (car args) 0)))))
    (fn-defmethod "tail" (seq-sym) (make-param-list :pos `((,seq-sym)))
      ((list-class-sym) (if (eq (car args) empty)
                            empty
                            (cdar args)))
      ((string-class-sym) (if (= (length (car args)) 0)
                              ""
                              (subseq (car args) 1))))

    (fn-defmethod "cons" (seq-sym) (make-param-list :pos `((,x-sym) (,seq-sym)))
      ((list-class-sym) (cons (car args) (cadr args)))
      ((string-class-sym) (if (and (string? (car args))
                                    (= (length (car args)) 1))
                               (concatenate 'string (car args) (cadr args))
                               (runtime-error
                                "cons: string-cons first argument not a length-1 string"))))

    (fn-defmethod "length" (seq-sym) (make-param-list :pos `((,seq-sym)))
      ((list-class-sym) (num (fnlist-length (car args))))
      ((string-class-sym) (num (length (car args)))))
    (fn-defmethod "empty?" (seq-sym) (make-param-list :pos `((,seq-sym)))
      ((list-class-sym) (bool (eq (car args) empty)))
      ((string-class-sym) (bool (eq (length (car args)) 0))))

    (fn-defmethod "equal-method" (obj-sym) (make-param-list :pos `((,obj-sym) (,x-sym)))
      (() (equalp (car args) (cadr args))))
    (fn-defmethod "add-method" (left-sym right-sym)
        (make-param-list :pos `((,left-sym) (,right-sym))))

    ;; FIXME: make behavior of general object printing better (param backsubstitution?)
    (fn-defmethod "show" (obj-sym) (make-param-list :pos `((,obj-sym)))
      ((bool-class-sym) (show (car args)))
      ((class-class-sym) (show (car args)))
      ((function-class-sym) (show (car args)))
      ((list-class-sym)
       (let ((*print-pprint-dispatch* (copy-pprint-dispatch)))
             (set-pprint-dispatch 'string #'format)
             (format nil
                     "[~/pprint-fill/]"
                     (fnmapcar (lambda (x)
                                 (fn.eval::call-fnmethod method (list x)))
                               (car args)))))
      ((module-class-sym) (show (car args)))
      ((method-class-sym) (show (car args)))
      ((null-class-sym) (show (car args)))
      ((num-class-sym) (show (car args)))
      ((string-class-sym) (show (car args)))
      ;; default object printer
      (() (let ((*print-pprint-dispatch* (copy-pprint-dispatch)))
             (set-pprint-dispatch 'string #'format)
             (format nil
                     "(~a ~/pprint-fill/)"
                     (sym-name (fnclass-name (fnobj-class (car args))))
                     (->> (car args)
                       (fnobj-contents)
                       (ht->plist)
                       (group 2)
                       (reverse)
                       (mapcar $(fn.eval::call-fnmethod
                                 method
                                 (list (cell-value (cadr $))))))))))

    (fn-defun "gensym" (= 0)
      (fngensym))

    (fn-defun "+" (>= 1)
      (unless (every #'num? args)
        (runtime-error "+: Arguments must be numbers"))
      (num (apply #'+ args)))
    (fn-defun "-" (>= 1)
      (unless (every #'num? args)
        (runtime-error "-: Arguments must be numbers"))
      (num (apply #'- args)))
    (fn-defun "/" (>= 1)
      (unless (every #'num? args)
        (runtime-error "/: Arguments must be numbers"))
      (num (apply #'/ args)))
    (fn-defun "*" (>= 1)
      (unless (every #'num? args)
        (runtime-error "*: Arguments must be numbers"))
      (num (apply #'* args)))
    (fn-defun "pow" (>= 1)
      (unless (every #'num? args)
        (runtime-error "pow: Arguments must be numbers"))
      (num (expt (car args) (cadr args))))

    (fn-defun ">" (>= 2)
      (bool (apply #'> args)))
    (fn-defun "<" (>= 2)
      (bool (apply #'< args)))
    (fn-defun ">=" (>= 2)
      (bool (apply #'>= args)))
    (fn-defun "<=" (>= 2)
      (bool (apply #'<= args)))

    (fn-defun "=" (= 2)
      (if (equalp (car args) (cadr args))
          true
          false))

    (fn-defun "print" (= 1)
      (princ (fn.eval::call-fnmethod (env-var *current-env* (fnintern "show")) args))
      fnnull)
    (fn-defun "println" (= 1)
      (princ (fn.eval::call-fnmethod (env-var *current-env* (fnintern "show")) args))
      (terpri)
      fnnull)
    (fn-defun "load" (= 1)
      (unless (string? (car args))
        (runtime-error "load: Argument must be a string"))
      (with-open-file (in (car args) :direction :input)
        (let ((ast (fn.parser:parse (fn.scanner:scan in (car args)))))
          (mapc $(handler-case (eval-ast $)
                   (fn-error (x)
                     (format t "load: Error encountered: ~a~%" x)))
                ast)))
      fnnull)
    (fn-defun "runtime-error" (= 1)
      (runtime-error "~a" (show (car args))))
    (fn-defun "exit" (>= 0)
      (funcall #'sb-ext:exit
               :code (if args (floor (car args)) 0)))

    ;; load sequence stuff
    ;;(fn.eval::eval-file )
    ))

(defun init-runtime ()
  "Initialize the fn runtime."
  (let* ((*runtime* (make-runtime))
         (*current-env* (init-env)))
    (init-runtime-hook)
    *runtime*))
