(defclass data ()
  ((type :initarg :type
         :documentation "The type of this instance")
   (dcons :initarg :dcons
          :documentation "The constructor name")
   (contents :initarg :contents
             :documentation "The vector containing this object's data")))

(defclass dcons-entry ()
  ((name :initarg :name)
   (type :initarg :type)
   (type-args :initarg :type-args)
   (arg-names :initarg :arg-names)))

;;(defmacro defschema ())
;;(defmacro new ())

