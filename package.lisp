(defpackage :fn-impl
  (:use :common-lisp)
  (:shadow type)
  (:export :boot-fn :unboot-fn :|unboot-fn| :fn-readtable :fn-read))

(defpackage :|fn|
  (:documentation "Built-in definitions for the fn programming language.")
  (:nicknames :fn))
