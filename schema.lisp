(in-package :fn-impl)


;;;;;;
;;; Schema classes
;;;;;;

(defclass abstract-schema ()
  ((name :initarg :name
         :type symbol
         :documentation "The symbol used by 'new to construct this object")
   (data-classes :initarg :data-classes
                 :type list
                 :documentation "The possible classes of objects constructed by
 this schema. Each type must be unique to this schema, as it is used to pair up
 schemas and instance objects."))

  (:documentation "Abstract interface to the schema class which allows them to
 be looked up in the schema table."))


(defclass general-schema (abstract-schema)
  ((construct :initarg :construct
              :type function
              :documentation "Function to construct this data. Takes arguments
 (&REST KEYS).")
   (get :initarg :get
        :type function
        :documentation "Function to get values from instances of this schema.
 Takes arguments (INSTANCE KEY).")
   (set :initarg :set
        :type function
        :documentation "Function to set values in instances of this schema.
 Takes arguments (INSTANCE KEY VALUE).")
   (match :initarg :match
          :type function
          :documentation "Function used to do matching and destructuring. It
 should return either NIL (no match) or a BINDINGS object. Takes arguments
 (PATTERN-ARGS OBJ). If NIL, then all pattern arguments are assumed to be
 patterns.")
   (pattern-vars :initarg :pattern-vars
                 :initform nil
                 :type (or function nil)
                 :documentation "NIL or a function that returns a list of
 symbols which would be bound on successful pattern matching. Takes
 arguments (PATTERN-ARGS)."))

  (:documentation "An ABSTRACT-SCHEMA subclass where schema operations are
 general functions"))


(defclass data-schema (abstract-schema)
  ((arg-list :initarg :arg-list
             :type list
             :documentation "Arg-list for the constructor, pattern matcher")
   (construct :initarg :construct
              :type function
              :documentation "Constructor for the schema")
   (slots :initarg :slots
          :type list
          :documentation "Slots in objects of this type")
   (options :initarg :options
            :type list
            :documentation "A list of options for this schema"))

  (:documentation "A schema which has a record structure defined by an
 arg-list"))

(defclass data-instance ()
  ((contents :initarg :contents
             :type dict
             :documentation "Dict containing object slots.")
   (schema :initarg :schema
           :type schema
           :documentation "Schema used to construct this instance."))

  (:documentation "Superclass for all instances created from data-schemas"))


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
  (declare (type abstract-schema schema))
  (with-slots (name data-classes) schema
    (if (gethash name schemas-by-name)
        (warn "Redefining schema named ~a" name))
    (setf (gethash name schemas-by-name) schema)
    (mapcar $(setf (gethash $ schemas-by-class) schema)
            (slot-value schema 'data-classes))))

(defun data-args-gen (a*)
  "Generates arguments for make-instance with the specified args list"
  (let ((x (arg-list-vars a*)))
    (mapcan $[`',$ $] x)))


;;;;;;
;;; Schema functions
;;;;;;

(defgeneric schema-construct (schema args)
  (:documentation "Create an object from the given schema"))
(defgeneric schema-get (schema instance slot)
  (:documentation "Get a value from an object built by schema"))
(defgeneric schema-set (schema instance slot value)
  (:documentation "Set a value in an object described by schema"))

;; general-schema methods
(defmethod schema-construct ((schema general-schema) args)
  (apply (slot-value schema 'construct) args))
(defmethod schema-get ((schema general-schema) instance slot)
  (funcall (slot-value schema 'get) instance slot))
(defmethod schema-set ((schema general-schema) instance slot value)
  (funcall (slot-value schema 'set) instance slot value))

;; schema methods
(defmethod schema-construct ((schema data-schema) args)
  (apply (slot-value schema 'construct) args))
(defmethod schema-get ((schema data-schema) instance key)
  (dict-get (slot-value instance 'contents) key))
(defmethod schema-set ((schema data-schema) instance key value)
  (setf (dict-get (slot-value instance 'contents) key) value))


