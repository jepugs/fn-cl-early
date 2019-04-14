(in-package :fn-impl)

;; True, False, and Null are special values which have their own type
(defclass fn-true () ())
(defclass fn-false () ())
(defclass fn-null () ())

(defvar fn-true (make-instance 'fn-true))
(defvar fn-false (make-instance 'fn-false))
(defvar fn-null (make-instance 'fn-null))

(defvar fn-true-sym '|fn|::|True|)
(defvar fn-false-sym '|fn|::|False|)
(defvar fn-null '|fn|::|Null|)

(defun fn-truthy (obj)
  "Tell whether an object should be considered to be True or False in fn."
  (if (or (eq obj fn-false)
          (eq obj fn-null))
      nil
      t))

(defmethod print-object ((object fn-true) stream)
  (princ "True" stream))
(defmethod print-object ((object fn-false) stream)
  (princ "False" stream))
(defmethod print-object ((object fn-null) stream)
  (princ "Null" stream))

;; wildcard pattern
(defparameter fn-wildcard (intern "_" :|fn|))
