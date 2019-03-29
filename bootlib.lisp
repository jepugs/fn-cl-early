;;;; bootlib.lisp -- definitions used to bootstrap the standard library
(in-package :fn-impl)

;;;;;;
;;; Macros for Bootstrap Definitions
;;;;;;

(defparameter bootlib-lexical-exports nil
  "Lexical variables which will be automatically copied to the fn package using def.
 These symbols will be copied using the def special form.")
(defparameter bootlib-macro-exports nil
  "Describes which symbols which should be copied to the fn package as macros.")
(defparameter bootlib-sym-macro-exports nil
  "Describes which symbols which should be copied to the fn package as symbol
 macros.")

;;; TODO: hook exports code into lexical definitions code to prevent people from
;;; redefining built-in forms.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun check-name-form (name-form function-name)
    "Ensures validity of the name forms in the /boot macros. function-name is used
 for generating the error message."
    (unless (or (symbolp name-form)
                (and (listp name-form)
                     (= (length name-form) 2)
                     (symbolp (car name-form))
                     (stringp (cadr name-form))))
      (error "~a: invalid name form ~s" function-name name-form))))

(defmacro def/boot (name-form value)
  "Define a lexical global and add it to the lexical export list. This is more
 restricted than the def operator in that it doesn't support pattern matching or
 multiple definitions."
  (check-name-form name-form "DEF/BOOT")
  (let* ((name (if (listp name-form) (car name-form) name-form)))
    `(progn (|def| ,name ,value)
            (push ',name-form bootlib-lexical-exports))))

(defmacro defn/boot (name-form arg-list &body body)
  "Define a lexical global function and add it to the lexical export list.
 This macro accepts the same arguments as defn."
  (check-name-form name-form "DEFN/BOOT")
  (let* ((name (if (listp name-form) (car name-form) name-form)))
    `(progn (|defn| ,name ,arg-list ,@body)
            (push ',name-form bootlib-lexical-exports))))

(defmacro defmacro/boot (name-form arg-list &body body)
  "Creates a macro definition and adds it to the export list. Unlike the
 other /BOOT macros, this uses a Common Lisp-style lambda list."
  (check-name-form name-form "DEFMACRO/BOOT")
  (let* ((name (if (listp name-form) (car name-form) name-form)))
    `(progn (defmacro ,name ,arg-list ,@body)
            (push ',name-form bootlib-macro-exports))))

(defmacro def-sym-macro/boot (name-form expansion)
  (check-name-form name-form "DEF-SYM-MACRO/BOOT")
  (let* ((name (if (listp name-form) (car name-form) name-form)))
    `(progn (define-symbol-macro ,name ,expansion)
            (push ',name-form bootlib-sym-macro-exports))))


;;;;;;
;;; null type
;;;;;;

(def-sym-macro/boot |null| '|null|)

;;;;;;
;;; our eponymous function maker
;;;;;;

(defmacro/boot |fn| (arg-list &body body)
  "Like lambda, but with different argument list and support for options"
  (let* ((lform `(lambda ,(convert-arg-list arg-list) |null| ,@body))
         (lopt (get-options body)))
    ;; IMPLNOTE: if at all possible, implement new options by expanding the
    ;; pipeline before
    (->> lform
      (funcall $(aif (dict-get lopt :memoize)
                     `(memoize ,$)
                     $))
      (funcall $(aif (dict-get lopt :curry)
                     (curry-transform it $)
                     $)))))


;;;;;;
;;; Quoting
;;;;;;

(defmacro/boot |quote| (thing)
  `(quote ,thing))


;;;;;;
;;; Definers
;;;;;;

(defmacro/boot |def| (&body pattern-value-pairs)
  "Define a constant in the global lexical environment."
  (let* ((pairs (group 2 pattern-value-pairs)))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       ,@(mapcar
          $(for-match-vars (car $)
                           (cadr $)
                           (lambda (k v)
                             `(define-lexically ,k ,v nil)))
          pairs)
       |null|)))

(defmacro/boot |def*| (&body pattern-value-pairs)
  "Define a global dynamic variable."
  (let* ((pairs (group 2 pattern-value-pairs)))
    `(progn
       ,@(mapcar $(for-match-vars (car $)
                                  (cadr $)
                                  (lambda (k v)
                                    `(defparameter ,k ,v)))
                 pairs)
       |null|)))

(defmacro/boot |defn| (name arg-list &body body)
  "Define a function in the global lexical environment."
  `(|def| ,name (|fn| ,arg-list ,@body)))

(defmacro/boot |defvar| (&body pattern-value-pairs)
  "Define a lexical variable."
  ;; same as def except we set the mutable flag to T
  (let* ((pairs (group 2 pattern-value-pairs)))
    `(progn ,@(mapcar $`(define-lexically ,(car $) ,(cadr $) t)
                      pairs)
            |null|)))

(defmacro/boot |let| (bindings &body body)
  (let* ((pairs (group 2 bindings))
         (patterns (mapcar #'car pairs))
         (objs (mapcar #'cadr pairs)))
    (pattern-assignments patterns
                         objs
                         `(progn ,@body)
                         `(error "let: match failed"))))

(defmacro/boot |set!| (name value)
  "Update the value of a named variable"
  ;; For now, this is all we need
  `(setf ,name ,value))


;;;;;;
;;; Booleans
;;;;;;

(def-sym-macro/boot |true| '|true|)
(def-sym-macro/boot |false| '|false|)

(defun true-ish (x)
  (not (eq x |false|)))
(defun false-ish (x)
  (eq x |false|))

(defun rebool (x)
  "Convert CL booleans to fn ones. Be careful using this, because it will
 interpret empty lists as false values and fn's |false| as a true value."
  (if x
      |true|
      |false|))

(defn/boot |is-bool| (x)
  (rebool (or (eq x |true|)
              (eq x |false|))))

(defn/boot |not| (x)
        (if (true-ish x)
            |false|
            |true|))

(defmacro/boot |and| (&rest args)
  (if args
      `(|if| ,(car args)
             (|and| ,@(cdr args))
             |false|)
      |true|))

(defmacro/boot |or| (&rest args)
  (if args
      `(|if| ,(car args)
             |true|
             (|or| ,@(cdr args)))
      |false|))

(defn/boot (|impl-=| "=") (x0 & x)
  {:curry 1}
  (rebool (apply #'equalp x0 x)))
(defn/boot (|impl-<| "<") (x0 & x)
  {:curry 1}
  (rebool (apply #'< x0 x)))
(defn/boot (|impl-<=| "<=") (x0 & x)
  {:curry 1}
  (rebool (apply #'<= x0 x)))
(defn/boot (|impl->| ">") (x0 & x)
  {:curry 1}
  (rebool (apply #'> x0 x)))
(defn/boot (|impl->=| ">=") (x0 & x)
  {:curry 1}
  (rebool (apply #'>= x0 x)))


;;;;;;
;;; Control Flow
;;;;;;

(defmacro/boot |do| (&body forms)
  `(progn
     |null|
     ,@forms))

(defmacro/boot |if| (test then else)
  `(if (true-ish ,test)
       ,then
       ,else))

;; FIXME: probably want to change this expansion so it's easier to understand
;; (a/o/t recursive expansion)
(defmacro/boot |cond| (&body clauses)
  (if clauses
      `(|if| ,(car clauses)
             ,(cadr clauses)
             (|cond| ,@(cddr clauses)))
      |null|))

(defmacro/boot |case| (obj &body clauses)
  "Perform pattern matching on OBJ. Each clause is a pattern and expression
pair. Patterns are tested in the order specified until the first match is found,
at which point the expression of the clause is executed."
  (let ((obj-var (gensym))
        (bid (gensym))
        (res (gensym)))
    `(let ((,obj-var ,obj))
       (block ,bid
         ,@(mapcar $(pattern-assignments [(car $)]
                                         [obj-var]
                                         `(let ((,res ,(cadr $)))
                                            (return-from ,bid ,res))
                                         nil)
                   (group 2 clauses))))))

;;;;;;
;;; Schemas
;;;;;;

(defmacro/boot |new| (name &rest args)
  `(|new*| ',name ,@args))

(defn/boot |new*| (name & args)
  (aif (gethash name schemas-by-name)
       (schema-construct it args)
       (error "new: unknown schema ~a" name)))

(defn/boot @ (i obj)
  (let ((x (class-name (class-of obj))))
    (aif (gethash x schemas-by-class)
         (schema-get it obj i)
         (error "@: ~a has unknown class" obj))))

(defun (setf @) (v i obj)
  (let ((x (class-name (class-of obj))))
    (aif (gethash x schemas-by-class)
         (schema-set it obj i v)
         nil)))

(defmacro/boot |defdata| (name arg-list)
  "Defines a new data-schema"
  `(progn
     (add-schema
      (make-instance
       'data-schema
       :name ',name
       :data-classes [',name]
       :arg-list ',arg-list
       :construct (|fn| ,arg-list
                    (make-instance
                     ',name
                     :contents {,@(data-args-gen arg-list)}))
       :slots (arg-list-vars ',arg-list)
       :options []))
     (defclass ,name (data-instance)
       ((schema :initform (gethash 'name schemas-by-name))))
     (defmethod print-object ((object ,name) stream)
       (format stream "(INSTANCE-OF '~s" ',name)
       ,@(mapcar $`(format stream " '~s ~s " ',$ (@ ',$ object))
                 (arg-list-vars arg-list))
       (write-char #\) stream))))


;;;;;;
;;; Lists
;;;;;;

(defn/boot |list| (& args)
        (apply #'list args))

(defn/boot |is-list| (x)
  (rebool (listp x)))

(defn/boot |cons| (hd & tl)
  {:curry 1}
  (if (listp tl)
      (cons hd tl)
      (error "cons: tail must be a list")))

(defn/boot |last| (l)
  (car (last l)))

(eval-when (:compile-toplevel :execute :load-toplevel)
  (add-schema
   (make-instance
    'general-schema
    :name '|list|
    :data-classes '(cons list null)
    :construct |list|
    :get (lambda (instance slot)
           (declare (type integer slot)
                    (type list instance))
           (nth slot instance))
    :set (lambda (instance slot value)
           (declare (type list instance)
                    (type integer slot))
           (setf (nth slot instance) value))
    :match (lambda (pattern-args obj)
             (when (listp obj)
               (rlambda (res a* x*) ({}  pattern-args obj)
                 (cond ((null a*) (if (null x*)
                                      res
                                      nil))
                       ((eq (car a*) '&)
                        (unless (eq (length a*) 2)
                          (error "list matching: wrong arguments after &~s"
                                 (cdr a*)))
                        (aif (pattern-match (cadr a*) x*)
                             (dict-extend res it)
                             nil))
                       ((null x*) nil)
                       (t (aif (pattern-match (car a*) (car x*))
                               (recur (dict-extend res it)
                                      (cdr a*)
                                      (cdr x*))
                               nil))))))
    :pattern-vars (lambda (pattern-args)
                    (mapcan #'pattern-vars
                            (remove-if $(eq $ '&) pattern-args))))))


;;;;;;
;;; Dicts
;;;;;;

(defn/boot |dict| (& kv-pairs)
  (apply #'dict kv-pairs))

(defn/boot |is-dict| (x)
  (rebool (is-dict x)))

(defn/boot |dict-keys| (d)
  (dict-keys d))

(defn/boot |dict-has-key| (d key)
  (rebool (dict-has-key d key)))

(defn/boot |dict-get| (d key)
  (dict-get d key))

(defn/boot |dict-conj| (dict key value)
  (dict-conj dict key value))

(defn/boot |dict-extend| (dict0 & dicts)
  (apply #'dict-extend dict0 dicts))

(eval-when (:compile-toplevel :execute :load-toplevel)
  (add-schema
   (make-instance
    'general-schema
    :name '|dict|
    :data-classes '(hash-table)
    :construct #'dict
    :get (lambda (instance slot)
           (declare (type dict instance))
           (dict-get instance slot))
    :set (lambda (instance slot value)
           (declare (type dict instance))
           (setf (dict-get instance slot) value))
    :match (lambda (pattern-args obj)
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
                      (mapcan $(pattern-vars (cadr $)) pairs))))))

;; dict pattern is
;; (dict KEYFORM PATTERN KEYFORM PATTERN ...)
;; KEYFORM := KEY | (KEY DEFAULT-VALUE)
;; recommended to use :keywords as dict keys


;;;;;;
;;; Strings
;;;;;;

(defn/boot |string| (& printables)
  (apply #'concatenate
         'string
         (mapcar $(with-output-to-string (princ $)) printables)))

(defn/boot |is-string| (x)
  (rebool (stringp x)))

(defn/boot |is-char| (x)
  (rebool (and (stringp x)
               (eql (length x) 1))))

(eval-when (:compile-toplevel :execute :load-toplevel)
  (add-schema
   (make-instance 'general-schema
                  :name '|string|
                  :data-classes '(string)
                  :construct #'|string|
                  :get (lambda (instance slot)
                         (declare (type string instance)
                                  (type integer slot))
                         (aref instance slot))
                  :set (lambda (instance slot value)
                         (declare (type string instance)
                                  (type integer slot)
                                  (type string value))
                         (concatenate 'string
                                      (subseq instance 0 slot)
                                      value
                                      (if (< slot (- (length instance) 1))
                                          (subseq instance (+ slot 1))
                                          "")))
                  :match (lambda (pattern-args obj)
                           (declare (ignore obj))
                           (error "String pattern is unimplemented: ~a"
                                  `(string ,@pattern-args)))
                  :pattern-vars (lambda (pattern-args)
                                  (declare (ignore pattern-args))
                                  nil))))


;;;;;;
;;; Numbers
;;;;;;

(defn/boot |is-int| (x)
  (rebool (integerp x)))

(defn/boot |is-float| (x)
  (rebool (floatp x)))

(defn/boot |is-num| (x)
  (rebool (numberp x)))


;;;;;;
;;; Sequences (in lieu of a real protocol)
;;;;;;

(defn/boot |is-sequence| (seq)
  (or (listp seq) (stringp seq) (is-dict seq)))

(defn/boot |length| (seq)
  (cond
    ((or (vectorp seq) (listp seq)) (length seq))
    ((is-dict seq) (hash-table-count seq))
    (t (error "length: not a sequence: ~s" seq))))

(defn/boot |is-empty| (seq)
  (= (length seq) 0))

(defn/boot |as-list| (seq)
  (cond
    ((listp seq) seq)
    ((stringp seq) (map 'list #'string seq))
    ((is-dict seq)
     (let (res)
       (maphash $(push [$0 $1] res) seq)
       (nreverse res)))
    (t (error "Not a sequence: ~s" seq))))

(defun match-seq-type (templ seq)
  "TEMPL and SEQ are sequences. Return a sequence with the type of TEMPL and the
 contents of SEQ."
  (cond
    ((listp templ) (|as-list| seq))
    ((stringp templ)
     (if (stringp seq)
         seq
         (apply #'concatenate 'string (|as-list| seq))))
    ((is-dict templ)
     (reduce $(apply #'dict-conj $0 $1)
             (mapcar #'|as-list| (|as-list| seq))
             :initial-value {}))))

(defn/boot |head| (seq)
  (cond ((listp seq) (car seq))
        ((stringp seq) (aref seq 0))
        ((is-dict seq)
         (block b
           (maphash $(return-from b [$0 $1]) seq)))
        (t (error "head: not a sequence"))))

(defn/boot |tail| (seq)
  (cond ((listp seq) (cdr seq))
        ((stringp seq) (subseq seq (min 1 (length seq))))
        ((is-dict seq) (match-seq-type {} (cdr (|as-list| seq))))
        (t (error "tail: not a sequence"))))

(defn/boot |split| (n seq)
  {:curry 1}
  (labels ((iter (acc tl m)
             (|if| (|and| (|not| (|is-empty| seq))
                          (|impl-<| m n))
                   (iter (cons (car tl) acc) (cdr tl) (+ m 1))
                   [(match-seq-type seq (nreverse acc))
                    (match-seq-type seq tl)])))
    (iter [] seq 0)))

(defn/boot |take| (n seq)
        {:curry 1}
        (subseq seq 0 (min n (length seq))))

(defn/boot |drop| (n seq)
  {:curry 1}
  (subseq seq (min n (length seq))))

(defn/boot |split-when| (f seq)
  {:curry 1}
  (labels ((iter (acc tl)
             (|if| (funcall f (car tl))
                   (iter (cons (car tl) acc) (cdr tl))
                   [(match-seq-type seq (nreverse acc))
                    (match-seq-type seq tl)])))
    (iter [] seq)))

(defn/boot |take-while| (f seq)
  {:curry 1}
  (labels ((iter (acc tl)
             (|if| (funcall f (car tl))
                   (iter (cons (car tl) acc) (cdr tl))
                   (match-seq-type seq (nreverse acc)))))
    (iter [] seq)))

(defn/boot |drop-while| (f seq)
  {:curry 1}
  (if (funcall f (|head| seq))
      (|drop-while| f (|tail| seq))
      seq))

(defn/boot |map| (f seq)
  {:curry 1}
  (match-seq-type seq (mapcar f (|as-list| seq))))

(defn/boot |zip| (& seqs)
  (apply #'map 'list |list| (mapcar |as-list| seqs)))

(defn/boot |filter| (f seq)
  {:curry 1}
  (match-seq-type seq (remove-if-not f (|as-list| seq))))

(defn/boot |fold| (f init seq)
  {:curry 2}
  (match-seq-type seq (reduce f (|as-list| seq) :initial-value init)))


;;;;;;
;;; I/O stuff
;;;;;;

(defn/boot |print| (x)
  (princ x))

