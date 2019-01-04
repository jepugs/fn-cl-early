(defclass normal-type ()
  ((tcons
    :initarg :tcons
    :type symbol
    :documentation "Type constructor")
   (args
    :initarg :args
    :type list
    :documentation "Argument types")))

(defmethod print-object ((obj normal-type) stream)
  (print-unreadable-object (obj stream :type t)
    (with-slots (tcons args)
        obj
      (format stream "~a~{ ~a~}" tcons args))))

(defun is-normal-type (obj)
  (eq (type-of obj) 'normal-type))

(defclass fn-type ()
  ((args
    :initarg :args
    :type list
    :documentation "Argument types")
   (res
    :initarg :res
    :type list
    :documentation "Result types")))

(defmethod print-object ((obj fn-type) stream)
  (print-unreadable-object (obj stream :type t)
    (with-slots (res args)
        obj
      (format stream "~{~a ~}-> ~a" args res))))

(defun tcons-of (type)
  (cond ((eq (type-of type) 'fn-type) '->)
        ((eq (type-of type) 'normal-type) (slot-value type 'tcons))
        (t nil)))

;; TODO: write this function
(defun type= (l r)
  (cond
    ((is-normal-type l)
     (and (is-normal-type r)
          (eq (tcons-of l) (tcons-of r))
          (let ((al (slot-value l 'args))
                (ar (slot-value r 'args)))
            (= (length al) (length ar))
            (every #'type= l r))))
    ((symbolp l) (eq l r))))


