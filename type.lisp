;;;; type.lisp -- implementation of object system
(in-package :fn-impl)


;;;;;;
;;; Type class definitions
;;;;;;

;;; A note on terminaology: slots and indices
;; Slots are the names of the in


(defclass type ()
  ((name :initarg :name
         :type symbol
         :documentation "The (unique) symbol identifying this type.")
   (classes :initarg :classes
            :type list
            :documentation "The classes which are considered to be of this type")
   (slots :initarg :slots
          :type list
          :documentation "Symbols naming the slots in this type.")
   (instantiator :initarg :instantiator
                 :type (or function boolean)
                 :documentation "Function to instantiate this type (not construct). Takes arguments
 described by the slots arg list in defdata. Should return an object whose class is in CLASSES.
 Should emit an error for uninstantiable objects. A value of T causes the default instantiator to be
 used.")
   (immutable :initarg :immutable
              :type boolean
              :documentation "Whether the slots of this object are immutable.")
   (constructor :initarg :constructor
                :type (or function null)
                :documentation "Function to construct this type.")
   (getter :initarg :getter
           :type function
           :documentation "Function to get values from instances of this type. Takes arguments
 (THIS SLOT).")
   (setter :initarg :setter
           :type function
           :documentation "Function to set values in instances of this type. Takes arugments
 (THIS SLOT VALUE).")
   ;; matching behavior is implemented mainly in match.lisp
   (matcher :initarg :matcher
            :type function
            :documentation "Function to do pattern matching on this type. Takes arguments
 (PATTERN-ARGS OBJ), and returns a dict of symbols and values that represents bindings.")
   (pattern-vars :initarg :pattern-vars
               :type function
               :documentation "Function that processes pattern args to find symbols which would
 be bound on successful pattern matching. Takes arguments (PATTERN-ARGS).")

   (impl-table :initarg :impl-table
               :initform (make-hash-table :test #'eq)
               :type hash-table
               :documentation "A table of protocols implemented by this method. This is a hierarchy
 of hash tables, with the keys being protocol-name (top level) -> method-name. The final value is a
 function implementing the corresponding method on this type. This will be mutated."))

  (:documentation "A data structure describing a type in fn."))

(defmethod print-object ((object type) stream)
  (format stream "(TYPE :NAME ~s ...)" (slot-value object 'name)))


(defclass fn-object ()
  ((type :initarg :type
         :type type
         :documentation "Type of this object")
   (contents :initarg :contents
             :type dict
             :documentation "Dict of slots."))

  (:documentation "Superclass for all user-defined types in fn"))

(defmethod print-object ((object fn-object) stream)
  (let ((type (fn-type-of object)))
    (format stream
            "(~s~{~^ ~s~})"
            (slot-value type 'name)
            (dict-values (slot-value object 'contents)))))

;;;;;;
;;; Type Functions
;;;;;;

(defun instantiate (name args)
  "Instantiate a type."
  (aif (gethash name types-by-name)
       (apply (slot-value it 'instantiator) args)
       (error "instantiate: unknown type ~s" name)))

(defun fn-slot-value (object slot)
  "Get the value of an slot"
  (dict-get (slot-value object 'contents) slot))

(defun set-slot (object slot value)
  "Set the value of an slot"
  (setf (dict-get (slot-value object 'contents) slot) value))

(defun index-value (object index)
  "Get the value at an index"
  (let ((name (class-name (class-of object))))
    (aif (gethash name types-by-class)
         (funcall (slot-value it 'getter) object index)
         (error "slot-value: can't find type for object of class ~s" name))))

(defun set-index (object index value)
  "Set the value at an index"
  (let ((name (class-name (class-of object))))
    (aif (gethash name types-by-class)
         (funcall (slot-value it 'setter) object index value)
         (error "slot-value: can't find type for object of class ~s" name))))


;;;;;;
;;; Type definition facilities
;;;;;;

;;; global tables used to access currently defined types
(defvar types-by-name (make-hash-table :test #'eq)
  "A hash table of types sorted by names")
(defvar types-by-class (make-hash-table :test #'eq)
  "A hash table of types sorted by data class")

(defun add-type (type)
  "Add a type to the global tables."
  (declare (cl:type type type))
  (with-slots (name classes) type
    (if (gethash name types-by-name)
        (warn "Redefining type named ~a" name))
    (setf (gethash name types-by-name) type)
    (mapcar $(setf (gethash $ types-by-class) type) classes)))

(defun fn-type-of (object)
  "Get the TYPE describing OBJECT"
  (if (typep object 'fn-object)
      (slot-value object 'type)
      (aif (gethash (class-name (class-of object)) types-by-class)
           it
           (error "FN-TYPE-OF: Could not determine type of object ~s" object))))


;;; default behaviors

(defun data-args-gen (arg-list)
  "Generates DICT arguments that bind arg list symbols to variables of the same name."
  (let ((x (arg-list-vars arg-list)))
    (mapcan $[`',$ $] x)))

(defun make-instantiator (name arg-list)
  "Generates default type instantiation code."
  (lambda (&rest args)
    (let ((contents (destructure-arg-list arg-list args)))
      (make-instance name
                     :type (gethash name types-by-name)
                     :contents contents))))

(defun make-constructor (name)
  "Generates default type constructor code."
  (lambda (&rest args)
    (funcall #'instantiate name args)))

;; the default getter and setter are not type-specific
(defun make-getter (name slots)
  (lambda (obj index)
    (if (member index slots)
        (fn-slot-value obj index)
        (error "~s is not a gettable index of object ~s (type ~s)." index obj name))))

(defun make-setter (name immutable slots)
  (if (fn-truthy immutable)
      (lambda (obj index value)
        (declare (ignore index value))
        (error "Object ~s of type ~s is immutable." obj name))
      (lambda (obj index value)
        (if (member index slots)
            (set-slot obj index value)
            (error "~s is not a settable index of object ~s (type ~s)." index obj name)))))

(defun make-matcher (arg-list type)
  "Creates a function to do arg list-based matching after checking that the object's type is
 contained in CLASSES."
  (lambda (pattern-args object)
    (when (eq (class-name (class-of object)) type)
      (let ((x (destructure-arg-list arg-list pattern-args))
            (res {}))
        (block b
          (maphash $(aif (pattern-match $1 (index-value object $0))
                         (setf res (dict-extend res it))
                         (return-from b nil))
                   x)
          res)))))

(defun make-pattern-vars (arg-list)
  "Creates a function to do arg list-based match var parsing."
  (lambda (pattern-args)
    (let ((x (destructure-arg-list arg-list pattern-args))
          (res []))
      (maphash $(setq res (append (pattern-vars $1) res)) x)
      res)))


;;; Methods used for False values in defdata
(defun make-false-instantiator (type)
  (lambda (&rest args)
    (declare (ignore args))
    (error "Type ~s is not instantiable." type)))
(defun make-false-getter (type)
  (lambda (obj index)
    (declare (ignore index))
    (error "Object ~s of type ~s has no gettable indices." obj type)))
(defun make-false-setter (type)
  (lambda (obj index value)
    (declare (ignore index value))
    (error "Object ~s of type ~s has no settable indices." obj type)))
(defun make-false-matcher (name)
  (lambda (pattern-args obj)
    (declare (ignore pattern-args obj))
    (error "Type ~s doesn't support matching." name)))
(defparameter false-pattern-vars
  (lambda (pattern-args)
    (declare (ignore pattern-args))
    nil))

;;; IMPLNOTE: This is a macro because we don't want to evaluate TRUE-FORM or FALSE-FORM if we won't
;;; use them.
(defmacro get-deftype-method (method instantiable true-form false-form)
  "If INSTANTIABLE is true and METHOD is True or False/Null, the respective form is returned.
 If INSTANTIABLE is false, FALSE-FORM is returned. Otherwise, METHOD is returned. This is used in
 the expansion of deftype to implement support for True/False method values."
  (let ((m (gensym)))
    `(if (fn-truthy ,instantiable)
         (let ((,m ,method))
           (cond ((eq ,m fn-true) ,true-form)
                 ((fn-truthy ,m) ,m)
                 (t ,false-form)))
         ,false-form)))

(defun make-deftype-type (name arg-list params)
  "Takes the (unevaluated) body of a deftype form and generates code to create a new TYPE with the
 specified properties."
  (let* ((slots (arg-list-vars arg-list))
         (instantiable `(dict-get ,params 'instantiable))
         (immutable `(dict-get ,params 'immutable)))
    `(make-instance 'type
                    :name ',name
                    :classes '(,name)
                    :slots ',slots
                    :instantiator (if (fn-truthy (dict-get ,params 'instantiable))
                                      (make-instantiator ',name ',arg-list)
                                      (make-false-instantiator ',name))
                    :immutable (fn-truthy ,immutable)
                    :constructor (get-deftype-method (dict-get ,params 'construct)
                                                     ,instantiable
                                                     (make-constructor ',name)
                                                     fn-false)
                    :getter (get-deftype-method (dict-get ,params 'get)
                                                ,instantiable
                                                (make-getter ',name ',slots)
                                                (make-false-getter ',name))
                    :setter (get-deftype-method (dict-get ,params 'set)
                                                ,instantiable
                                                (make-setter ',name
                                                             ,immutable
                                                             ',slots)
                                                (make-false-setter ',name))
                    :matcher (get-deftype-method (dict-get ,params 'match)
                                                 ,instantiable
                                                 (make-matcher ',arg-list ',name)
                                                 (make-false-matcher ',name))
                    :pattern-vars (get-deftype-method (dict-get ,params 'pattern-vars)
                                                    ,instantiable
                                                    (make-pattern-vars ',arg-list)
                                                    false-pattern-vars))))

(defun expand-deftype (name arg-list dt-body)
  "Expands a deftype form into code to create and bind the type."
  (let ((type (gensym))
        ;; get unevaluated forms for each argument
        (param-forms (destructure-arg-list `(:|instantiable| (instantiable fn-true)
                                              :|immutable| (immutable fn-true)
                                              :|construct| (construct fn-true)
                                              :|get| (get fn-true)
                                              :|set| (set fn-true)
                                              :|match| (match fn-true)
                                              :|pattern-vars| (pattern-vars fn-true))
                                           dt-body))
        ;; this variable will hold a dict of evaluated parameters
        (params (gensym)))
    `(let* ((,params (dict
                      ,@(mapcan $`(',(car $) ,(cadr $))
                                (dict->list param-forms))))
            (,type ,(make-deftype-type name arg-list params)))
       ;; create a class for the type
       (defclass ,name (fn-object) ())
       ;; add the type. There is only one class, and it has the same name as the type.
       (add-type ,type)
       ;; define the constructor.
       (when (fn-truthy (slot-value ,type 'constructor))
         (define-lexically ,name (slot-value ,type 'constructor) nil)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Primitive type definitions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-type
 (make-instance 'type
                :name '|fn|::|Float|
                :classes '(double-float single-float)
                :slots ()
                :immutable t
                :instantiator (make-false-instantiator '|fn|::|Float|)
                :constructor nil
                :getter (make-false-getter '|fn|::|Float|)
                :setter (make-false-setter '|fn|::|Float|)
                :matcher (make-false-matcher '|fn|::|Float|)
                :pattern-vars false-pattern-vars))

(add-type
 (make-instance 'type
                :name '|fn|::|Int|
                :classes '(fixnum bignum integer)
                :slots ()
                :immutable t
                :instantiator (make-false-instantiator '|fn|::|Int|)
                :constructor nil
                :getter (make-false-getter '|fn|::|Int|)
                :setter (make-false-setter '|fn|::|Int|)
                :matcher (make-false-matcher '|fn|::|Int|)
                :pattern-vars false-pattern-vars))

(add-type
 (make-instance 'type
                :name '|fn|::|String|
                ;; This is not portable, but it is what's right
                #+sbcl
                :classes '(sb-kernel:simple-character-string string)
                :slots ()
                :immutable t
                :instantiator (make-false-instantiator '|fn|::|String|)
                :constructor nil
                :getter (make-false-getter '|fn|::|String|)
                :setter (make-false-setter '|fn|::|String|)
                :matcher (make-false-matcher '|fn|::|String|)
                :pattern-vars false-pattern-vars))

(add-type
 (make-instance 'type
                :name '|fn|::|Symbol|
                :classes '(symbol)
                :slots ()
                :immutable t
                :instantiator (make-false-instantiator '|fn|::|Symbol|)
                :constructor nil
                :getter (make-false-getter '|fn|::|Symbol|)
                :setter (make-false-setter '|fn|::|Symbol|)
                :matcher (make-false-matcher '|fn|::|Symbol|)
                :pattern-vars false-pattern-vars))


(add-type
 (make-instance 'type
                :name '|fn|::|Bool|
                :classes '(fn-true fn-false)
                :slots ()
                :immutable t
                :instantiator (make-false-instantiator '|fn|::|Bool|)
                :constructor nil
                :getter (make-false-getter '|fn|::|Bool|)
                :setter (make-false-setter '|fn|::|Bool|)
                :matcher (make-false-matcher '|fn|::|Bool|)
                :pattern-vars false-pattern-vars))

(add-type
 (make-instance 'type
                :name '|fn|::|Null|
                :classes '(fn-null)
                :slots ()
                :immutable t
                :instantiator (make-false-instantiator '|fn|::|Null|)
                :constructor nil
                :getter (make-false-getter '|fn|::|Null|)
                :setter (make-false-setter '|fn|::|Null|)
                :matcher (make-false-matcher '|fn|::|Null|)
                :pattern-vars false-pattern-vars))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Non-primitive Built-in Types
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-type
 (make-instance
  'type
  :name '|fn|::|List|
  :classes '(cons list null)
  :slots ()
  :immutable nil
  :instantiator (make-false-instantiator '|fn|::|List|)
  :constructor nil
  :getter (lambda (obj index)
            (declare (cl:type list obj)
                     (cl:type integer index))
            (nth index obj))
  :setter (lambda (obj index value)
            (declare (cl:type list obj)
                     (cl:type integer index))
            (setf (nth index obj) value))
  :matcher (lambda (pattern-args obj)
             (when (listp obj)
               ;; iterator over pattern args, collecting bindings until either a match fails or we
               ;; run out of bindings
               (rlambda (res a* x*) ({}  pattern-args obj)
                 (cond ((null a*) (if (null x*)
                                      res
                                      nil))
                       ;; check for rest argument
                       ((eq (car a*) '&)
                        (unless (eq (length a*) 2)
                          (error "list matching: wrong arguments after &~s" (cdr a*)))
                        (aif (pattern-match (cadr a*) x*)
                             (dict-extend res it)
                             nil))
                       ;; object too short
                       ((null x*) nil)
                       ;; attempt matching and append new bindings
                       (t (aif (pattern-match (car a*) (car x*))
                               (recur (dict-extend res it)
                                      (cdr a*)
                                      (cdr x*))
                               nil))))))
  :pattern-vars (lambda (pattern-args)
                (mapcan #'pattern-vars
                        (remove-if $(eq $ '&) pattern-args)))))

(add-type
 (make-instance
  'type
  :name '|fn|::|Dict|
  :classes '(hash-table)
  :immutable nil
  :instantiator (make-false-instantiator '|fn|::|Dict|)
  :constructor nil
  :getter (lambda (instance slot)
            (declare (cl:type dict instance))
            (dict-get instance slot))
  :setter (lambda (instance slot value)
            (declare (cl:type dict instance))
            (setf (dict-get instance slot) value))
  :matcher (lambda (pattern-args obj)
             (when (is-dict obj)
               ;; this generates an error when there are illegal args
               (let ((x (apply #'dict pattern-args))
                     (res {}))
                 (block b
                   (maphash $(aif (pattern-match $1 (dict-get obj $0))
                                  (setq res (dict-extend res it))
                                  (return-from b nil))
                            x)
                   res))))
  :pattern-vars (lambda (pattern-args)
                  (let ((pairs (group 2 pattern-args)))
                    (unless (= (length (car (last pairs))) 2)
                      (error "dict pattern: Odd number of args"))
                    (mapcan $(pattern-vars (cadr $)) pairs)))))

;; dict pattern is
;; (dict KEYFORM PATTERN KEYFORM PATTERN ...)
;; KEYFORM := KEY | (KEY DEFAULT-VALUE)
;; recommended to use :keywords as dict keys


