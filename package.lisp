(defpackage :fn-impl
  (:use :common-lisp)
  (:export :@ :$ :& :_ :and-then :def :defn :defschema :dict :fn :match :my-cond
           :my-let :new))

;; symbols from CL we don't export in FN
(defparameter cl-omit [:defun :let :let* :defvar :defparameter])

;; all symbols exported from CL
(defparameter cl-exports
  (->$ (find-package :cl)
    (slot-value $ 'sb-impl::external-symbols)
    (slot-value $ 'sb-impl::cells)
    (remove-if-not #'symbolp $)
    (map 'list $(intern (symbol-name $)
                        (find-package :keyword))
         $)))

(defpackage :fn
  (:use :fn-impl :common-lisp)
  (:shadow :let :cond)
  ;;(:shadow :defun :defparameter :defvar :let :let* :labels :flet)
  (:export
   ;; FN special symbols/operators
   :@ :$ :& :_ :and-then :cond :defn :defschema :dict :fn :let :match :new
   ;; Things from common lisp
   #.(remove-if (lambda (x) (member x cl-omit :test #'eq))
                cl-exports)))


