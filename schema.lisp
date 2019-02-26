(in-package :fn-impl)

(load "macros.lisp")


;; destructure an FN-style arguments list
;;(defun )


(defclass abstract-schema ()
  ((name :initarg :name
         :type symbol
         :documentation "The symbol used by 'new to construct this object")
   (data-classes :initarg :data-classes
                 :type lists
                 :documentation "The possible classes of objects constructed by
 this schema. Each type must be unique to this schema, as it is used to pair up
 schemas and instance objects."))

  (:documentation "Abstract interface to the schema class which allows them to
 be looked up in the schema table."))

(defclass general-schema (abstract-schema)
  ((construct :initarg :construct
              :type function
              :documentation "Function to construct this data. Takes arguments
 (&REST FIELDS).")
   (get :initarg :get
        :type function
        :documentation "Function to get field values from instances of this
 schema. Takes arguments (INSTANCE FIELD).")
   (set :initarg :set
        :type function
        :documentation "Function to set field values in instances of this
 schema. Takes arguments (INSTANCE FIELD VALUE).")
   (match :initarg :match
          :type function
          :documentation "Function used to do matching and destructuring. It
 should return either NIL (no match) or a BINDINGS object. Takes arguments
 (MATCH-ARGS OBJ)."))

  (:documentation "An ABSTRACT-SCHEMA subclass where schema operations are
 general functions."))


(defclass schema (abstract-schema)
  ((fields :initarg :fields
           :type list
           :documentation "ARG-LIST of fields in the schema."))

  (:documentation "A schema with the standard record structure"))

(defgeneric schema-construct (schema args))
(defgeneric schema-get (schema instance field))
(defgeneric schema-set (schema instance field value))
;; note: schema-match is only called after it is established that OBJ has the
;; type described by the schema
(defgeneric schema-match (schema match-args obj))

;; general-schema methods
(defmethod schema-construct ((schema general-schema) args)
  (apply (slot-value schema 'construct) args))
(defmethod schema-get ((schema general-schema) instance field)
  (funcall (slot-value schema 'get) instance field))
(defmethod schema-set ((schema general-schema) instance field value)
  (funcall (slot-value schema 'set) instance field value))
(defmethod schema-match ((schema general-schema) match-args obj)
  (funcall (slot-value schema 'match) match-args obj))

;; schema methods
(defmethod schema-construct ((schema schema) args)
  (with-slots (data-classes fields) schema
    (apply #'make-instance (car data-classes) (mapcan #'list fields args))))
(defmethod schema-get ((schema schema) instance field)
  (slot-value instance field))
(defmethod schema-set ((schema schema) instance field value)
  (setf (slot-value instance field) value))

(defun xor (a b)
  (or (and a (not b))
      (and b (not a))))

(defmethod schema-match ((schema schema) match-args obj)
  (labels ((recur (patterns fields res)
             (when (xor patterns fields)
               nil)
             (if fields
                 (aif (pattern-match (car patterns)
                                     (slot-value obj (car fields)))
                      (recur (cdr patterns)
                             (cdr fields)
                             (nconc res
                                    (bindings-alist it)))
                      nil)
                 (make-instance 'bindings :alist res))))
    (recur match-args (slot-value schema 'fields) nil)))


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

(defmacro defschema (name &body fields)
  `(progn (defclass ,name ()
            (,@(mapcar $`(,$ :initarg ,$)
                       fields)))
          (add-schema
           (make-instance 'schema
                          :name ',name
                          :data-classes (list ',name)
                          :fields ',fields))))

(defun @ (obj i)
  (let ((x (class-name (class-of obj))))
    (aif (gethash x schemas-by-class)
         (schema-get it obj i)
         (error "@: ~a has unknown class" obj))))

(defmacro new (name &rest args)
  `(new* ',name ,@args))

(defun new* (name &rest args)
  (aif (gethash name schemas-by-name)
       (schema-construct it args)
       (error "new: unknown schema ~a" name)))

;; add the list schema
(add-schema
 (make-instance
  'general-schema
  :name 'list
  :data-classes '(cons list null)
  :construct #'list
  ;; FIXME: accessor gives null when field is outside its range
  :get (lambda (instance field)
         (declare (type integer field))
         (nth field instance))
  :set (lambda (instance field value)
         (setf (nth field instance) value))
  :match (lambda (match-args obj)
           (labels ((recur (patterns tail res)
                      (if patterns
                          (if (eq (car patterns) '&)
                              (aif (pattern-match (cadr patterns) tail)
                                   (bindings-conc it res)
                                   nil)
                              (aif (pattern-match (car patterns) (car tail))
                                   (recur (cdr patterns)
                                          (cdr tail)
                                          (bindings-conc it res))
                                   nil))
                          res)))
             (recur match-args obj (make-instance 'bindings))))))
