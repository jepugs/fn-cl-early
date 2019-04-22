;;;; protocol.lisp -- implementation of protocols

(in-package :fn-impl)

(defparameter this-symbol '|fn|::|this|)

;;; Having an entire class for method descriptions seems like overkill for now, but I want the
;;; freedom to make methods more complicated in the future (e.g. add smarter type checking)

(defclass method-spec ()
  ((name :initarg :name
         :type symbol
         :documentation "The name of this method.")
   (arg-list :initarg :arg-list
             :type list
             :documentation "The argument list of this method. Must have length >=1."))

  (:documentation "Method prototype used to create implementations"))

(defclass protocol ()
  ((name :initarg :name
         :type symbol)
   (methods :initarg :methods
            :type list
            :documentation "List of METHOD-SPECs describing the methods of this protocol.")))

(defparameter protocol-table (make-hash-table :test #'eq)
  "All globally-defined protocols, by name.")

(defun add-impl! (type protocol method-table)
  "Add an implementation of a protocol to a type. This mutates TYPE. METHOD-TABLE should be a hash
 table keyed by the methods names in the protocol."
  ;; verify that all the methods are here. One day we may verify more
  (let ((names (mapcar $(slot-value $ 'name) (slot-value protocol 'methods)))
        (pname (slot-value protocol 'name))
        (itab (slot-value type 'impl-table)))
    (unless (every $(gethash $ method-table) names)
      (error "ADD-IMPL: Missing entries in METHOD-TABLE."))
    ;; add an implementation hash table to TYPE
    (setf (gethash pname itab) (make-hash-table))
    ;; add the methods
    (mapcar $(setf (gethash $0 (gethash pname itab)) (gethash $0 method-table)) names)))

(defun get-method (obj protocol method)
  "Get the function implementing a method for an object OBJ. Returns NIL on failure."
  (declare (cl:type symbol protocol method))
  (let* ((type (fn-type-of obj))
         (impl (gethash protocol (slot-value type 'impl-table))))
    (if impl
        ;; at this point, we are guaranteed that the method exists unless some ne'er-do-well has
        ;; been messing about with things manually
        (gethash method impl)
        ;; no implementation => error
        (error "Object ~s (type ~s) does not implement ~s" obj type protocol))))

(defun make-method-dispatch-function (proto-name method-spec)
  "Create a (Common Lisp) function that invokes the named fn method."
  (let ((method-name (slot-value method-spec 'name)))
    (lambda (&rest args)
      (apply (get-method (car args) proto-name method-name) args))))

(defun expand-defproto (proto-name body)
  "Expand a defproto form"
  (let ((method-specs (mapcar $(make-instance 'method-spec
                                              :name (first $)
                                              :arg-list (second $))
                              (group 2 body))))
    `(progn
       ,@(mapcar $`(define-lexically ,(slot-value $ 'name)
                                     (make-method-dispatch-function ',proto-name ,$)
                                     nil)
                 method-specs)
       (setf (gethash ',proto-name protocol-table)
             (make-instance 'protocol
                            :name ',proto-name
                            :methods (list ,@method-specs))))))

(defun expand-defimpl (proto-name type-name body)
  "Expand a defimpl form"
  (let ((method-table (gensym))
        (dict-args (mapcan $(list (list 'QUOTE (first $)) (second $))
                           (group 2 body))))
   `(let ((,method-table (dict ,@dict-args)))
      (add-impl! (gethash ',type-name types-by-name)
                 (gethash ',proto-name protocol-table)
                 ,method-table))))
