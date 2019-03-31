(defpackage :fn-impl
  (:use :common-lisp)
  (:export :boot-fn :unboot-fn :|unboot-fn| :fn-readtable :fn-read))

(defpackage :|fn|
  (:documentation "Built-in definitions for the fn programming language.")
  (:nicknames :fn))
