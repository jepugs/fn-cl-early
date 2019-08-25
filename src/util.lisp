;;;; util.lisp -- Utility library used throughout the implementation of fn

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

(defpackage :fn.util
  (:documentation "Utility library used throughout the implementation of fn.")
  (:use :cl)
  (:export
   ;; List/sequence functions
   :group :take :drop :split :take-while :drop-while :split-when :split-at :flatten :interleave
   :length< :length= :set-equal
   ;; hash-table functions
   :make-ht :make-eq-ht :ht-keys :ht-has-key :ht-values :ht->plist :ht-conc :ht-append :ht-del-keys
   :copy-ht
   ;; control-flow macros
   :aif :it :rlambda :recur :-> :->> :->as
   ;; misc functionality
   :defconstant-1 :symb :quoted-p :name-eq :macroexpand-all :with-gensyms :xor
   ;; error handling
   :fn-error :origin :make-origin :origin-filename :origin-line :origin-column :filename
   :line :column :macro :origin->string  :message :module))

(in-package :fn.util)

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

(defun split-at (elt lst &key (test #'eql) (repeat t))
  "Split at a delimiter. The delimiter is removed from the resulting list. If REPEAT is T (default),
 then the list is partitioned into one more list than the number of delimiters."
  (if repeat
      (let ((x (split-when (lambda (x)
                             (funcall test elt x))
                           lst)))
        ;; check if any split occurred
        (if (cadr x)
            ;; FIXME: this should maybe be a tail recursive call but fuck it
            ;; use CDADR instead of CADR to drop the delimiter
            (cons (car x) (split-at elt (cdadr x) :test test :repeat t))
            ;; don't include the trailing empty list
            (list (car x))))
      (let ((x (split-when (lambda (x)
                             (funcall test elt x))
                           lst)))
        (if (cadr x)
            ;; return the same thing but without the delimiter element
            (list (car x) (cdadr x))
            ;; include the empty second list
            x))))

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

(defun length< (lst n)
  "Tell if the length of LST is less than N without computing the full length of LST. This is useful
 when N is small and LST might be very long."
  (null (drop (- n 1) lst)))

(defun length= (lst n)
  "Tell if the length of LST is less than N without computing the full length of LST. This is useful
 when N is small and LST might be very long."
  (rlambda (n lst) (n lst)
      (cond ((zerop n) (null lst))
            ((null lst) nil)
            (t (recur (- n 1) (cdr lst))))))

(defun set-equal (lst0 lst1)
  "Tell if two lists contain the same elements"
  (not (or (set-difference lst0 lst1 :test #'equalp)
           (set-difference lst1 lst0 :test #'equalp))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Hash table functions

(defun make-ht (&rest keys-and-values)
  "Create a new hash table. Arguments are alternating keys and values (i.e. a PLIST). New hash
 tables default to using FN= for their test (so they can have FN-OBJECTs as keys)."
  (let ((kv (group 2 keys-and-values))
        (res (make-hash-table :test 'equalp)))
    (mapcar (lambda (x)
              (setf (gethash (car x) res) (cadr x)))
            kv)
    res))

(defun make-eq-ht (&rest keys-and-values)
  "Create a hash table that tests keys with EQ. KEYS-AND-VALUES is a PLIST used to initialize the
contents of the table."
  (let ((kv (group 2 keys-and-values))
        (res (make-hash-table :test 'eq)))
    (mapcar (lambda (x)
              (setf (gethash (car x) res) (cadr x)))
            kv)
    res))


;; change how we print hash tables so we can see their contents
(defmethod print-object ((ht hash-table) stream)
  (format stream "{~{~s~^ ~%~}}" (ht->plist ht)))


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

(defun ht-del-keys (ht keys &key (test #'eql))
  "Non-destructively delete all the keys in the list KEYS from HT. TEST is a test function used to
  determine whether a key is a member of KEYS."
  (declare (cl:type hash-table ht)
           (cl:type list keys)
           (cl:type function test))
  (let ((res (make-ht)))
    (maphash (lambda (k v)
               (unless (member k keys :test test)
                 (setf (gethash k res) v)))
             ht)
    res))

(defun copy-ht (ht)
  (let ((res (make-hash-table :test (hash-table-test ht)
                              :rehash-size (hash-table-rehash-size ht)
                              :rehash-threshold (hash-table-rehash-threshold ht)
                              :size (hash-table-size ht))))
    (maphash (lambda (k v)
               (setf (gethash k res) v))
             ht)
    res))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Macro-writing Facilities

(defun symb (&rest args)
  "Make a symbol by concatenating ARGS as a string"
  (let ((strs (mapcar #'princ-to-string args)))
    (intern (apply #'concatenate 'string strs))))

(defun quoted-p (expr)
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

(defmacro with-gensyms (vars &body body)
  `(let ,(mapcar (lambda (var)
                   `(,var (gensym)))
                 vars)
     ,@body))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Misc functions & macros

(defmacro defconstant-1 (name value)
  "Like DEFCONSTANT but will not redefine a value on multiple successive calls."
  `(unless (boundp ',name)
     (defconstant ,name ,value)))

(defun xor (a b)
  (or (and a (not b))
      (and b (not a))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Error generation

;; origins contain information about where tokens, AST, and/or code objects originated. The MACRO
;; field is only relevant for code objects, but if provided indicates the name of the macro which
;; expanded into said code object (in this case, the line and column values will be the same as the
;; expression which yielded the macroexpansion in the first place).
(defstruct origin
  (filename nil)
  (line nil)
  (column nil)
  (macro nil))

(defun origin->string (o)
  (with-slots (filename line column macro) o
    (format nil "line ~a, col ~a~@[, file \"~a\"~]~@[, expansion of macro \"~a\"~]"
            line
            column
            filename
            macro)))

(define-condition fn-error (error)
  ((module :initarg :module
           :type string
           :documentation "The part of the interpreter in which the error occurred.")
   (origin :initarg :origin
           :documentation "Place in the code where the problem occurred.")
   (message :initarg :message
            :type string
            :documentation "Message to print")))

(defmethod print-object ((object fn-error) stream)
  (let ((*standard-output* stream))
    (with-slots (module origin message) object
      (princ module)
      (princ " error at ")
      (princ (origin->string origin))
      (princ ":")
      (terpri)
      (princ "    ")
      (princ message)
      (finish-output))))

(defmacro fn-error (origin fmt-string &rest fmt-args)
  `(error 'fn-error
          :module ,(package-name *package*)
          :origin ,origin
          :message (format nil ,fmt-string ,@fmt-args)))
