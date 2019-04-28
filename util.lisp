;;;; util.lisp -- utility code used throughout the implementation of fn

;;;; This file is part of fn.

;;;; fn is free software: you can redistribute it and/or modify it under the terms of the GNU
;;;; General Public License as published by the Free Software Foundation, either version 3 of the
;;;; License, or (at your option) any later version.

;;;; fn is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even
;;;; the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;;;; General Public License for more details.

;;;; You should have received a copy of the GNU General Public License along with fn. If not, see
;;;; <https://www.gnu.org/licenses/>.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :fn-impl)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; List functions

(defun group (n list)
  "Partition LIST into sublists of length N"
  (labels ((recur (m tail res)
             (if (zerop m)
                 (cons (nreverse res) (group n tail))
                 (recur (1- m) (cdr tail) (cons (car tail) res)))))
    (if list
        (recur n list nil)
        nil)))


(defun take (n lst)
  "Take the first n entries from a list"
  (loop for x in lst
     for i from 1 to n
     collect x))

(defun drop (n lst)
  "Drop the first N terms from LST. Equivalent to NTHCDR"
  (nthcdr n lst))

(defun split (n lst)
  "Split LST after the Nth element. Equal to (but faster than) (LIST (TAKE N LST) (DROP N LST))."
  (do ((m n (1- m))
       (right lst (cdr right))
       (left nil (cons (car right) left)))
      ((zerop m)
       (list (nreverse left) right))))

(defun take-while (test lst)
  "Take entries from LST until exhausted or (FUNCALL TEST (CAR LST)) returns NIL."
  (loop for x in lst
     while (funcall test x)
     collect x))

(defun drop-while (test lst)
  "Drop entries from LST until (FUNCALL TEST (CAR LST)) returns NIL."
  (loop for x in lst
     while (funcall test x)
     collect x))

(defun split-when (test lst)
  "Split LST at the first element that satisfies TEST. Returns [LEFT RIGHT]."
  (rlambda (rleft right) (nil lst)
    (cond ((null right)
           (list (nreverse rleft) right))
          ((funcall test (car right))
           (list (nreverse rleft) right))
          (t
           (recur (cons (car right) rleft) (cdr right))))))

(defun flatten (tree)
  "Turn TREE into a list of atoms by recursive list splicing. Returns a list even if TREE is an
 atom, e.g. (FLATTEN 'A) => (A)."
  (cond ((listp tree) (mapcan #'flatten tree))
        #+sbcl
        ;; in SBCL, comma syntax is represented by a special object
        ((sb-impl::comma-p tree) (flatten (sb-impl::comma-expr tree)))
        (t (list tree))))

(defun interleave (&rest lsts)
  "Interleave returns a list containing all the first elements of LSTs in the order provided, then
 the second elements, and so on, until a list is exhausted."
  (flatten (apply #'mapcar #'list lsts)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Hash table functions

;; IMPLNOTE: This is defined here so it can be used as a hash table test. It's a bit dicey because
;; the other type-related definitions are in type.lisp.

(defun fn= (x0 &rest x)
  "Equality that knows how to descend into fn objects."
  ;; IMPLNOTE: since EQUALP works on everything _but_ general objects, we have to manually check the
  ;; slots of instances of FN-OBJECT
  ;; IMPLNOTE: the definition of FN-OBJECT is in type.lisp
  (if (every $(typep $ 'fn-object) (cons x0 x))
      (with-slots (type contents) x0
        (every $(and (eq (slot-value $ 'type) type)
                     (equalp (slot-value $ 'contents) contents))
               x))
      (apply #'equalp x0 x)))

(defun fnhash (x)
  "Hash that understands fn objects"
  (if (typep x 'fn-object)
      (with-slots (type contents) x0
        (sxhash (list (slot-value type 'name) contents)))
      (sxhash x)))

;; add FN= as a hashtable test
(sb-ext:define-hash-table-test fn= fnhash)

(defun make-ht (&rest keys-and-values)
  "Create a new hash table. Arguments are alternating keys and values (i.e. a PLIST). New hash
 tables default to using FN= for their test (so they can have FN-OBJECTs as keys)."
  (let ((kv (group 2 keys-and-values))
        (res (make-hash-table :test 'fn=)))
    (mapcar (lambda (x)
              (setf (gethash (car x) res) (cadr x)))
            kv)
    res))


;; change how we print hash tables so they look like dicts in fn
(defmethod print-object ((ht hash-table) stream)
  (format stream "{~{~s~^ ~}}" (ht->plist ht)))


;; The NREVERSE calls in these functions maintain the order of the keys in the table so that MAKE-HT
;; and HT->PLIST are inverse operations. It works this way in SBCL.

(defun ht-keys (ht)
  "Get a list of keys in a hash table."
  (let ((res nil))
    (maphash (lambda (k v)
               (declare (ignore v))
               (push k res))
             ht)
    (nreverse res)))

(defun ht-has-key (ht key)
  "Tell if a key is contained in a hash table."
  (maphash (lambda (k v)
             (declare (ignore v))
             ;; check for key equality using the test from ht
             (if (funcall (hash-table-test ht) k key)
                 (return-from ht-has-key t)))
           ht)
  nil)

(defun ht-values (ht)
  "Get a list of values in a hash table."
  (let ((res nil))
    (maphash (lambda (k v)
               (declare (ignore k))
               (push v res))
             ht)
    (nreverse res)))

(defun ht->plist (ht)
  "Get a hash table as a plist."
  (let ((res nil))
    (maphash (lambda (k v)
               (push k res)
               (push v res))
             ht)
    (nreverse res)))

(defun ht-conc (ht0 &rest hts)
  "Non-destructively concatenate several hash tables. On key collision, use the value from the last
 provided hash table containing the key."
  (if hts
      (let ((res (make-hash-table :test (hash-table-test ht0))))
        (mapcar (lambda (ht)
                  (maphash (lambda (k v) (setf (gethash k res) v)) ht))
                (cons ht0 hts))
        res)))

(defun ht-append (ht key value)
  "Non-destructively append a single kv pair to a hash table."
  (let ((res (copy-structure ht)))
    (setf (gethash key res) value)
    res))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Control Flow Macros

(defmacro aif (test then else)
  "Anaphoric IF binds the value of TEST to IT in the clauses."
  `(let ((it ,test))
     (if it ,then ,else)))

(defmacro rlambda (llist args &body body)
  "Creates a recursive lambda form and calls it with ARGS. The lambda form can be called recursively
 from within the body via the name RECUR."
  `(labels ((recur ,llist
              ,@body))
     (recur ,@args)))

(defmacro -> (expr &body thread-forms)
  "Threading macro. Takes one "
  (reduce (lambda (inner outer)
            (if (symbolp outer)
                (list outer inner)
                `(,(car outer) ,inner ,@(cdr outer))))
          thread-forms
          :initial-value expr))

(defmacro ->> (expr &body thread-forms)
  (reduce (lambda (inner outer)
            (append outer (list inner)))
          (mapcar (lambda (x)
                    (if (symbolp x)
                        (list x)
                        x))
                  thread-forms)
          :initial-value expr))

(defmacro ->as (expr var &body thread-forms)
  (reduce (lambda (inner outer)
            (substitute-if
             outer
             (lambda (x) (eq x var))
             inner))
          thread-forms
          :initial-value expr))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Macro-writing Facilities

(defun symb (&rest args)
  "Make a symbol by concatenating ARGS as a string"
  (let ((strs (mapcar #'princ-to-string args)))
    (intern (apply #'concatenate 'string strs))))

(defun is-quoted (expr)
  "Tell if EXPR is a quoted expression."
  (and (listp expr)
       (eq (car expr) 'quote)
       (= (length expr) 2)))

(defun name-eq (sym1 sym2)
  "Check symbol name equality"
  (string= (symbol-name sym1) (symbol-name sym2)))

(defun macroexpand-all (tree)
  "Recursively macroexpand a form before evaluation. This changes macro semantics
 slightly, (particularly involving special forms like DECLARE), but we're lighting fires here
 anyway."
  (if (listp tree)
      (let ((tree0 (macroexpand tree)))
        (if (listp tree)
            (cons (car tree0)
                  (mapcar #'macroexpand-all (cdr tree0)))
            tree))
      tree))

(defmacro with-gensyms ())


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Misc functions

(defun xor (a b)
  (or (and a (not b))
      (and b (not a))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Deprecated functionality

;;; Delimiter Readers

(define-condition unmatched-close-delimiter (error)
  ((char :initarg :char))
  (:documentation "Condition signaled when a closing delimiter is encountered."))

(defmacro set-del-reader (open close operator)
  "Set delimiter reader. OPEN & CLOSE must be delimiters. Operator is prepended
to the list read in. e.g. (set-del-reader #\[ #\] list) causes [args] to be
read as (list args)."
  (let ((closer (intern (concatenate 'string (list close) "-reader")))
        (opener (intern (concatenate 'string (list open) "-reader"))))
    `(progn
       (defun ,opener (stream char)
         (declare (ignore char))
         (let (res)
           (loop
              (handler-case (push (read stream) res)
                (unmatched-close-delimiter (c)
                  (if (eql (slot-value c 'char) ,close)
                      (->> (nreverse res)
                        (cons ,operator)
                        (return))))))))
       (defun ,closer (stream char)
         (declare (ignore stream))
         (error 'unmatched-close-delimiter :char char))
       (set-macro-character ,open #',opener)
       (set-macro-character ,close #',closer))))

(eval-when (:load-toplevel)
  (set-del-reader #\[ #\] 'list))
(eval-when (:load-toplevel)
  (set-del-reader #\{ #\} 'make-ht))


;;; DICT functions

(defun dict (&rest keys-and-values)
  "Create a new dict. Arguments are alternating keys and values (i.e. a PLIST). New dicts default to
using EQUALP for their test."
  (let ((kv (group 2 keys-and-values))
        (res (make-hash-table :test #'equalp)))
    (mapcar (lambda (x)
              (setf (gethash (car x) res) (cadr x)))
            kv)
    res))

;; define dict as a synonym for hash table
(deftype dict () 'hash-table)

(defun is-dict (obj)
  (hash-table-p obj))

(defun dict-get (dict k &optional (default nil default-p))
  "Get the value associated with the key K from DICT."
  (multiple-value-bind (v exists) (gethash k dict)
    (if exists
        v
        (if default-p
            default
            (error "DICT-GET: ~s doesn't contain key ~s" dict k)))))

(defun (setf dict-get) (v dict k)
  "Set the value associated with the key K from DICT."
  (setf (gethash k dict) v))

(defun dict-keys (dict)
  "Get a list of keys in a dict."
  (let ((res nil))
    (maphash (lambda (k v)
               (declare (ignore v))
               (push k res))
             dict)
    (nreverse res)))

(defun dict-has-key (dict key)
  "Tell if a key is contained in a dict."
  (maphash (lambda (k v)
             (declare (ignore v))
             ;; check for key equality using the test from dict
             (if (funcall (hash-table-test dict) k key)
                 (return-from dict-has-key t)))
           dict)
  nil)

(defun dict-values (dict)
  "Get a list of values in a dict."
  (let ((res nil))
    (maphash (lambda (k v)
               (declare (ignore k))
               (push v res))
             dict)
    (nreverse res)))

(defun dict->list (dict)
  "Get a dict as a plist."
  (let ((res nil))
    (maphash (lambda (k v)
               (push (list k v) res))
             dict)
    (nreverse res)))

(defun dict-extend (dict0 &rest dicts)
  "Non-destructively concatenate several dicts. On key collision, use the value from the last
 provided dict containing the key."
  (if dicts
      (let ((res (make-hash-table :test (hash-table-test dict0))))
        (mapcar (lambda (dict)
                  (maphash (lambda (k v) (setf (gethash k res) v)) dict))
                (cons dict0 dicts))
        res)))

(defun dict-conj (dict key value)
  "Non-destructively append a single kv pair to a dict."
  (let ((res (copy-structure dict)))
    (setf (gethash key res) value)
    res))

;;; old name
(defun split-if (test lst)
  "Split LST at the first element that satisfies TEST. Returns [LEFT RIGHT]."
  (rlambda (rleft right) (nil lst)
    (cond ((null right)
           (list (nreverse rleft) right))
          ((funcall test (car right))
           (list (nreverse rleft) right))
          (t
           (recur (cons (car right) rleft) (cdr right))))))
