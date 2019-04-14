;;; Purely Functional Dictionary Class
(in-package :fn-impl)

(defun dict (&rest kv-pairs)
  "Create a new dict object"
  (let ((kv (group 2 kv-pairs))
        (res (make-hash-table :test #'equal)))
    (mapcar $(setf (gethash (car $) res) (cadr $)) kv)
    res))

(defun is-dict (d)
  "Tell if an object is a dict."
  (hash-table-p d))


(defun dict-keys (d)
  "Get all defined keys in a dictionary."
  (let ((res []))
    (maphash (lambda (k v)
               (declare (ignore v))
               (push k res))
             d)
    res))


(defun dict-has-key (d key)
  "Tell if a key is in a dict."
  (member key (dict-keys d) :test #'equal))


(defun dict-get (d key)
  "Get a value from a dict."
  (multiple-value-bind (val bound) (gethash key d)
    (if bound
        val
        (error "dict-get: can't find key in dict: ~a" key))))


(defun (setf dict-get) (val d key)
  "Set a value in a dict."
  (setf (gethash key d) val))

(defun dict-conj (dict key value)
  (let ((res (copy-structure dict)))
    (setf (dict-get res key) value)
    res))

(defun dict-extend (dict0 &rest dicts)
  "Extend dict1 with the keys contained in dicts. This operation is purely
 functional. dict1 is extended one dictionary at a time from left to right in
 the args list, overwriting any duplicated keys."
  (if dicts
      (let ((dict1 (dict)))
        ;; insert the elements from the first two dicts
        (maphash $(setf (gethash $0 dict1) $1) dict0)
        (maphash $(setf (gethash $0 dict1) $1) (car dicts))
        (apply #'dict-extend dict1 (cdr dicts)))
      dict0))

(defun dict->list (d)
  (let (res)
    (maphash (lambda (k v)
               (push (list k v) res))
             d)
    (nreverse res)))

(deftype dict () 'hash-table)

(defmethod print-object ((object hash-table) stream)
  (write-char #\{ stream)
  (maphash (lambda (k v)
             (format stream "~s ~s " k v))
           object)
  (write-char #\} stream))
