(in-package :fn-impl)


;;;;;;
;;; Schema classes
;;;;;;

;;; A note on terminaology: slots
;; The word slot is used to refer to two related but distinct concepts. Every object in fn has a set
;; of what we call _internal slots_, which hold the values intrinsic to each instance of the object.
;; In addition, _external slots_ are what we call the values used to address objects using the
;; universal accessor `@'. For example, while a dict has no actual internal slots, every key in the
;; dictionary is treated like a slot. Internal slots are accessed using `slot-value', and external
;; slots are accessed using `@'.

(defclass schema ()
  ((name :initarg :name
         :type symbol
         :documentation "The (unique) symbol identifying this schema.")
   (classes :initarg :classes
            :type list
            :documentation "Symbols naming the Common Lisp classes belonging to this schema.")
   (internal-slots :initarg :internal-slots
                   :type list
                   :documentation "Symbols naming the internal slots in this schema.")
   (instantiator :initarg :instantiator
                 :type function
                 :documentation "Function to instantiate this schema (not construct). Takes arguments
 described by the slots arg list in defdata. Should return an object whose class is in CLASSES.")
   (getter :initarg :getter
           :type function
           :documentation "Function to get values from instances of this schema. Takes arguments
 (THIS SLOT).")
   (setter :initarg :setter
           :type function
           :documentation "Function to set values in instances of this schema. Takes arugments
 (THIS SLOT VALUE).")
   ;;; matching behavior is implemented mainly in match.lisp
   (matcher :initarg :matcher
            :type function
            :documentation "Function to do pattern matching on this schema. Takes arguments
 (PATTERN-ARGS OBJ), and returns a dict of symbols and values that represents bindings.")
   (match-var-parser :initarg :match-var-parser
                     :type function
                     :documentation "Function that processes pattern args to find symbols which would
 be bound on successful pattern matching. Takes arguments (PATTERN-ARGS)."))

  (:documentation "A data structure describing a type in fn."))

(defclass fn-object ()
  ((contents :initarg :contents
             :type dict
             :documentation "Dict of internal slots."))

  (:documentation "Superclass for all user-defined types in fn"))


;;;;;;
;;; Schema Functions
;;;;;;

(defun new (name args)
  "Instantiate a schema."
  (aif (gethash name schemas-by-name)
       (apply (slot-value it 'instantiator) args)
       (error "new: unknown schema ~s" name)))

(defun internal-slot-value (object slot)
  "Get the value of an internal slot"
  (dict-get (slot-value object 'contents) slot))

(defun set-internal-slot (object slot value)
  "Set the value of an internal slot"
  (setf (dict-get (slot-value object 'contents) slot) value))

(defun external-slot-value (object slot)
  "Get the value of an external slot"
  (let ((name (class-name (class-of object))))
    (aif (gethash name schemas-by-class)
         (funcall (slot-value it 'getter) object slot)
         (error "slot-value: can't find schema for object of class ~s" name))))

(defun set-external-slot (object slot value)
  "Set the value of an external slot"
  (let ((name (class-name (class-of object))))
    (aif (gethash name schemas-by-class)
         (funcall (slot-value it 'setter) object slot value)
         (error "slot-value: can't find schema for object of class ~s" name))))


;;;;;;
;;; Schema definition facilities
;;;;;;

;;; global tables used to access currently-defined schemas
(defvar schemas-by-name (make-hash-table :test #'eq)
  "A hash table of schemas sorted by names")
(defvar schemas-by-class (make-hash-table :test #'eq)
  "A hash table of schemas sorted by data class")

(defun add-schema (schema)
  "Add a schema to the global tables."
  (declare (type schema schema))
  (with-slots (name classes) schema
    (if (gethash name schemas-by-name)
        (warn "Redefining schema named ~a" name))
    (setf (gethash name schemas-by-name) schema)
    (mapcar $(setf (gethash $ schemas-by-class) schema) classes)))


;;; default behaviors

(defun data-args-gen (arg-list)
  "Generates DICT arguments that bind arg list symbols to variables of the same name."
  (let ((x (arg-list-vars arg-list)))
    (mapcan $[`',$ $] x)))

(defun gen-instantiator (name arg-list)
  "Generates default schema instantiation code."
  (lambda (&rest args)
    (let ((contents (destructure-arg-list arg-list args)))
      (make-instance name :contents contents))))

;; the default getter and setter are not schema-specific
(defparameter default-getter #'internal-slot-value)
(defparameter default-setter #'set-internal-slot)

(defun make-matcher (arg-list classes)
  "Creates a function to do arg list-based matching after checking that the object's type is contained
 in CLASSES."
  (lambda (pattern-args object)
    (when (member (class-name (class-of object))
                  classes)
      (let ((x (destructure-arg-list arg-list pattern-args))
            (res {}))
        (block b
          (maphash $(aif (pattern-match $1 (external-slot-value object $0))
                         (setf res (dict-extend res it))
                         (return-from b nil))
                   x)
          res)))))

(defun make-match-var-parser (arg-list)
  "Creates a function to do arg list-based match var parsing."
  (lambda (pattern-args)
    (let ((x (destructure-arg-list arg-list pattern-args))
          (res []))
      (maphash $(setq res (append (pattern-vars $1) res)) x)
      res)))

(defun gen-matcher (arg-list classes)
  "Generates default schema matching code."
  (make-matcher arg-list classes))

(defun gen-match-var-parser (arg-list)
  "Generates default schema match var parsing code."
  (make-match-var-parser arg-list))


;;; Methods used for Null values in defdata
(defparameter null-getter
  (lambda (obj slot)
    (declare (ignore slot))
    (error "Object ~s has no external slots." obj)))
(defparameter null-setter
  (lambda (obj slot value)
    (declare (ignore slot value))
    (error "Object ~s has no settable slots." obj)))
(defun gen-null-matcher (name)
  (lambda (pattern-args obj)
    (declare (ignore pattern-args obj))
    (error "Schema ~s doesn't support matching." name)))
(defparameter null-match-vars
  (lambda (pattern-args)
    (declare (ignore pattern-args))
    nil))

(defun get-defdata-function (key body null-value default-value)
  "Looks for KEY in a defdata body. If it's set to Null, returns NULL-VALUE. If it's not present,
 returns DEFAULT-VALUE. Otherwise, it returns a function whose arg list is the CADR of the option form
 and whose body is the CDDR."
  (aif (assoc key body)
       (if (eq '|fn|::|Null| (cadr it))
           null-value
           (make-fn (cadr it) (cddr it)))
       default-value))

(defun make-defdata-schema (name arg-list body)
  "Makes a schema given the name, arg list, and body of a defdata form."
  (let ((instantiator (gen-instantiator name arg-list))
        (getter (get-defdata-function :|get| body null-getter default-getter))
        (setter (get-defdata-function :|set| body null-setter default-setter))
        (matcher (get-defdata-function :|match|
                                       body
                                       (gen-null-matcher name)
                                       (gen-matcher arg-list (list name))))
        (match-var-parser (get-defdata-function :|match-vars|
                                                body
                                                null-match-vars
                                                (gen-match-var-parser arg-list))))
    (make-instance 'schema
                   :name name
                   :classes (list name)
                   :internal-slots (arg-list-vars arg-list)
                   :instantiator instantiator
                   :getter getter
                   :setter setter
                   :matcher matcher
                   :match-var-parser match-var-parser)))

(defun make-defdata-constructor (name arg-list body)
  "Generates a constructor for the function. Returns NIL for a Null constructor."
  (let ((constructor-form (assoc :|construct| body)))
    (if constructor-form
        (if (eq (cadr constructor-form) '|fn|::|Null|)
            nil
            (make-fn (cadr constructor-form) (caddr constructor-form)))
        (gen-instantiator name arg-list))))

(defun expand-defdata (name arg-list body)
  "Expands a defdata form into code that creates the schema."
  (let ((a (gensym))
        (b (gensym))
        (constructor (gensym))
        (schema (gensym)))
    `(let* ((,a ',arg-list)
            (,b ',body)
            (,constructor (make-defdata-constructor ',name ,a ,b))
            (,schema (make-defdata-schema ',name ,a ,b)))
       (add-schema ,schema)
       (eval-when (:compile-toplevel :load-toplevel :execute)
        (defclass ,name (fn-object) ()))
       (defmethod print-object ((object ,name) stream)
         (print (cons ',name (replace-vars ,a (slot-value object 'contents))) stream))
       (if ,constructor
           (define-lexically ,name ,constructor nil))
       fn-null)))
