(in-package :fn-impl)

;; True, False, and Null are special values which have their own type
(defclass fn-true () ())
(defclass fn-false () ())
(defclass fn-null () ())

(defvar fn-true (make-instance 'fn-true))
(defvar fn-false (make-instance 'fn-false))
(defvar fn-null (make-instance 'fn-null))

(defmethod print-object ((object fn-true) stream)
  (princ "True" stream))
(defmethod print-object ((object fn-false) stream)
  (princ "False" stream))
(defmethod print-object ((object fn-null) stream)
  (princ "Null" stream))

;; wildcard pattern
(defparameter fn-wildcard (intern "_" :|fn|))
