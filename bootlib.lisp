;;;; bootlib.lisp -- definitions used to bootstrap the standard library
(in-package :fn-impl)

;;;;;;
;;; Macros for Bootstrap Definitions
;;;;;;

(defparameter bootlib-defs nil
  "Def forms for the fn bootstrap environment.")

;;; TODO: hook exports code into lexical definitions code to prevent people from
;;; redefining built-in forms.

(defmacro def/boot (name value)
  "Define a lexical global and add it to the lexical export list. This is more
 restricted than the def operator in that it doesn't support pattern matching or
 multiple definitions."
  `(progn
     (push '(|fn|::|def| ,(intern name "fn") ,value) bootlib-defs)))

(defmacro defn/boot (name arg-list &body body)
  "Define a lexical global function and add it to the lexical export list.
 This macro accepts the same arguments as defn."
  `(progn
     (push '(|fn|::|defn| ,(intern name "fn") ,arg-list ,@body) bootlib-defs)))

(defmacro defmacro/boot (name arg-list &body body)
  "Creates a macro definition and adds it to the export list. Unlike the
 other /BOOT macros, this uses a Common Lisp-style lambda list."
  `(progn
     (push '(defmacro ,(intern name "fn") ,arg-list ,@body) bootlib-defs)))

(defmacro def-sym-macro/boot (name expansion)
  `(progn
     (push '(define-symbol-macro ,(intern name "fn") ,expansion) bootlib-defs)))


;;;;;;
;;; null type
;;;;;;

(def-sym-macro/boot "Null" fn-null)


;;;;;;
;;; our eponymous function maker
;;;;;;

(defmacro/boot "fn" (arg-list &body body)
  "Like lambda, but with different argument list and support for options"
  (make-fn arg-list body)
  (let* ((lform `(lambda ,(convert-arg-list arg-list) fn-null ,@body))
         (lopt (get-options body)))
    ;; IMPLNOTE: if at all possible, implement new options by expanding the
    ;; pipeline before
    (->> lform
      (funcall $(if (and (dict-get lopt :|memoize|))
                    `(memoize ,$)
                    $))
      (funcall $(aif (dict-get lopt :|curry|)
                     (curry-transform it $)
                     $)))))


;;;;;;
;;; Control Flow
;;;;;;

(defmacro/boot "do" (&body forms)
  `(progn
     fn-null
     ,@forms))

(defmacro/boot "if" (test then else)
  `(if (true-ish ,test)
       ,then
       ,else))

;; FIXME: probably want to change this expansion so it's easier to understand
;; (a/o/t recursive expansion)
(defmacro/boot "cond" (&body clauses)
  (if clauses
      `(|fn|::|if| ,(car clauses)
            ,(cadr clauses)
            (|fn|::|cond| ,@(cddr clauses)))
      fn-null))

(defmacro/boot "case" (obj &body clauses)
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
;;; Quoting
;;;;;;

(defmacro/boot "quote" (thing)
  `(quote ,thing))


;;;;;;
;;; Definers
;;;;;;

(defmacro/boot "def" (&body pattern-value-pairs)
  "Define a constant in the global lexical environment."
  (let* ((pairs (group 2 pattern-value-pairs)))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       ,@(mapcar
          $(for-match-vars (car $)
                           (cadr $)
                           (lambda (k v)
                             `(define-lexically ,k ,v nil)))
          pairs)
       fn-null)))

(defmacro/boot "def*" (&body pattern-value-pairs)
  "Define a global dynamic variable."
  (let* ((pairs (group 2 pattern-value-pairs)))
    `(progn
       ,@(mapcar $(for-match-vars (car $)
                                  (cadr $)
                                  (lambda (k v)
                                    `(defparameter ,k ,v)))
                 pairs)
       fn-null)))

(defmacro/boot "defn" (name arg-list &body body)
  "Define a function in the global lexical environment."
  `(|fn|::|def| ,name (|fn|::|fn| ,arg-list ,@body)))

(defmacro/boot "defvar" (&body pattern-value-pairs)
  "Define a lexical variable."
  ;; same as def except we set the mutable flag to T
  (let* ((pairs (group 2 pattern-value-pairs)))
    `(progn ,@(mapcar $`(define-lexically ,(car $) ,(cadr $) t)
                      pairs)
            fn-null)))

(defmacro/boot "let" (bindings &body body)
  (let* ((pairs (group 2 bindings))
         (patterns (mapcar #'car pairs))
         (objs (mapcar #'cadr pairs)))
    (pattern-assignments patterns
                         objs
                         `(progn ,@body)
                         `(error "let: match failed"))))

(defmacro/boot "set!" (name value)
  "Update the value of a named variable"
  ;; For now, this is all we need
  `(setf ,name ,value))


;;;;;;
;;; Booleans
;;;;;;

(def-sym-macro/boot "True" fn-true)
(def-sym-macro/boot "False" fn-false)

(defun true-ish (x)
  (not (eq x fn-false)))
(defun false-ish (x)
  (eq x fn-false))

(defun rebool (x)
  "Convert CL booleans to fn ones. Be careful using this, because it will
 interpret empty lists as false values and fn's fn-false as a true value."
  (if x
      fn-true
      fn-false))

(defn/boot "is-bool" (x)
  (rebool (or (eq x fn-true)
              (eq x fn-false))))

(defn/boot "not" (x)
  (if (true-ish x)
      fn-false
      fn-true))

(defmacro/boot "and" (&rest args)
  (if args
      `(|fn|::|if| ,(car args)
            (|fn|::|and| ,@(cdr args))
            fn-false)
      fn-true))

(defmacro/boot "or" (&rest args)
  (if args
      `(|fn|::|if| ,(car args)
            fn-true
            (|fn|::|or| ,@(cdr args)))
      fn-false))

(defn/boot "=" (x0 & x)
  {:|curry| 1}
  (rebool (apply #'equalp x0 x)))
(defn/boot "<" (x0 & x)
  {:|curry| 1}
  (rebool (apply #'< x0 x)))
(defn/boot "<=" (x0 & x)
  {:|curry| 1}
  (rebool (apply #'<= x0 x)))
(defn/boot ">" (x0 & x)
  {:|curry| 1}
  (rebool (apply #'> x0 x)))
(defn/boot ">=" (x0 & x)
  {:|curry| 1}
  (rebool (apply #'>= x0 x)))


;;;;;;
;;; Schemas
;;;;;;

(defn/boot "instantiate" (name & args)
  (instantiate name args))

(defn/boot "slot-value" (object slot)
  (fn-slot-value object slot))

(defun (setf |fn|::|slot-value|) (value object slot)
  (set-slot object slot value))

(defn/boot "@" (object slot)
  (index-value object slot))

(defun (setf |fn|::@) (value object index)
  (set-index object index value))

(defmacro/boot "deftype" (name arg-list &body body)
  "Defines a new data-schema"
  (expand-deftype name arg-list body))


;;;;;;
;;; Lists
;;;;;;

(defn/boot "List" (& args)
        (apply #'list args))

(defn/boot "is-list" (x)
  (rebool (listp x)))

(defn/boot "cons" (hd & tl)
  {:|curry| 1}
  (if (listp tl)
      (cons hd tl)
      (error "cons: tail must be a list")))

(defn/boot "last" (l)
  (car (last l)))

(push
 `(add-type
   (make-instance
    'type
    :name '|fn|::|List|
    :slots ()
    :immutable nil
    :instantiator |fn|::|List|
    :constructor |fn|::|List|
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
    :match-vars (lambda (pattern-args)
                  (mapcan #'pattern-vars
                          (remove-if $(eq $ '&) pattern-args))))
   '(cons list null))
 bootlib-defs)


;;;;;;
;;; Dicts
;;;;;;

(defn/boot "Dict" (& kv-pairs)
  (apply #'dict kv-pairs))

(defn/boot "is-dict" (x)
  (rebool (is-dict x)))

(defn/boot "dict-keys" (d)
  (dict-keys d))

(defn/boot "dict-has-key" (d key)
  (rebool (dict-has-key d key)))

(defn/boot "dict-get" (d key)
  (dict-get d key))

(defn/boot "dict-conj" (dict key value)
  (dict-conj dict key value))

(defn/boot "dict-extend" (dict0 & dicts)
  (apply #'dict-extend dict0 dicts))

(push
 `(add-schema
   (make-instance
    'schema
    :name '|fn|::|Dict|
    :classes '(hash-table)
    :instantiator |fn|::|Dict|
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
    :match-var-parser (lambda (pattern-args)
                        (let ((pairs (group 2 pattern-args)))
                          (unless (= (length (car (last pairs))) 2)
                            (error "dict pattern: Odd number of args"))
                          (mapcan $(pattern-vars (cadr $)) pairs)))))
 bootlib-defs)

;; dict pattern is
;; (dict KEYFORM PATTERN KEYFORM PATTERN ...)
;; KEYFORM := KEY | (KEY DEFAULT-VALUE)
;; recommended to use :keywords as dict keys


;;;;;;
;;; Strings
;;;;;;

(defn/boot "String" (& printables)
  (apply #'concatenate
         'string
         (mapcar $(with-output-to-string (s) (princ $ s)) printables)))

(defn/boot "is-string" (x)
  (rebool (stringp x)))

(defn/boot "is-char" (x)
  (rebool (and (stringp x)
               (eql (length x) 1))))

(push
 '(add-schema
   (make-instance 'schema
                  :name '|fn|::|String|
                  :classes '(string)
                  :instantiator |fn|::|String|
                  :getter (lambda (instance slot)
                            (declare (cl:type string instance)
                                     (cl:type integer slot))
                            (aref instance slot))
                  :setter (lambda (instance slot value)
                            (declare (cl:type string instance)
                                     (cl:type integer slot)
                                     (cl:type string value))
                            (concatenate 'string
                                         (subseq instance 0 slot)
                                         value
                                         (if (< slot (- (length instance) 1))
                                             (subseq instance (+ slot 1))
                                             "")))
                  :matcher (lambda (pattern-args obj)
                             (declare (ignore obj))
                             (error "String pattern is unimplemented: ~a"
                                    `(string ,@pattern-args)))
                  :match-var-parser (lambda (pattern-args)
                                      (declare (ignore pattern-args))
                                      nil)))
 bootlib-defs)


;;;;;;
;;; Numbers
;;;;;;

(defn/boot "is-int" (x)
  (rebool (integerp x)))

(defn/boot "is-float" (x)
  (rebool (floatp x)))

(defn/boot "is-num" (x)
  (rebool (numberp x)))

(defn/boot "+" (x0 & x)
  {:|curry| 1}
  (apply #'+ x0 x))
(defn/boot "-" (x0 & x)
  {:|curry| 1}
  (apply #'- x0 x))
(defn/boot "*" (x0 & x)
  {:|curry| 1}
  (apply #'* x0 x))
(defn/boot "/" (x0 & x)
  {:|curry| 1}
  (apply #'/ x0 x))
(defn/boot "mod" (n d)
  {:|curry| 1}
  (mod n d))


;;;;;;
;;; Sequences (in lieu of a real protocol)
;;;;;;

(defn/boot "is-sequence" (seq)
  (or (listp seq) (stringp seq) (is-dict seq)))

(defn/boot "length" (seq)
  (cond
    ((or (vectorp seq) (listp seq)) (length seq))
    ((is-dict seq) (hash-table-count seq))
    (t (error "length: not a sequence: ~s" seq))))

(defn/boot "is-empty" (seq)
  (= (length seq) 0))

(defun as-list (seq)
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
    ((listp templ) (as-list seq))
    ((stringp templ)
     (if (stringp seq)
         seq
         (apply #'concatenate 'string (as-list seq))))
    ((is-dict templ)
     (reduce $(apply #'dict-conj $0 $1)
             (mapcar #'as-list (as-list seq))
             :initial-value {}))))

(defn/boot "head" (seq)
  (cond ((listp seq) (car seq))
        ((stringp seq) (aref seq 0))
        ((is-dict seq)
         (block b
           (maphash $(return-from b [$0 $1]) seq)))
        (t (error "head: not a sequence"))))

(defn/boot "tail" (seq)
  (cond ((listp seq) (cdr seq))
        ((stringp seq) (subseq seq (min 1 (length seq))))
        ((is-dict seq) (match-seq-type {} (cdr (as-list seq))))
        (t (error "tail: not a sequence"))))

(defn/boot "split" (n seq)
  {:|curry| 1}
  (labels ((iter (acc tl m)
             (|fn|::|if| (|fn|::|and| (|fn|::|not| (|fn|::|is-empty| seq))
                              (|fn|::|<| m n))
                  (iter (cons (car tl) acc) (cdr tl) (+ m 1))
                  [(match-seq-type seq (nreverse acc))
                   (match-seq-type seq tl)])))
    (iter [] seq 0)))

(defn/boot "take" (n seq)
  {:|curry| 1}
  (subseq seq 0 (min n (length seq))))

(defn/boot "drop" (n seq)
  {:|curry| 1}
  (subseq seq (min n (length seq))))

(defn/boot "split-when" (f seq)
  {:|curry| 1}
  (labels ((iter (acc tl)
             (|fn|::|if| (funcall f (car tl))
                  (iter (cons (car tl) acc) (cdr tl))
                  [(match-seq-type seq (nreverse acc))
                   (match-seq-type seq tl)])))
    (iter [] seq)))

(defn/boot "take-while" (f seq)
  {:|curry| 1}
  (labels ((iter (acc tl)
             (|fn|::|if| (funcall f (car tl))
                  (iter (cons (car tl) acc) (cdr tl))
                  (match-seq-type seq (nreverse acc)))))
    (iter [] seq)))

(defn/boot "drop-while" (f seq)
  {:|curry| 1}
  (|fn|::|cond|
    (|fn|::|is-empty| seq) seq
    (funcall f (|fn|::|head| seq))
      (|fn|::|drop-while| f (|fn|::|tail| seq))
    fn-true seq))

(defn/boot "map" (f seq)
  {:|curry| 1}
  (match-seq-type seq (mapcar f (as-list seq))))

(defn/boot "zip" (& seqs)
  (apply #'map 'list #'list (mapcar #'as-list seqs)))

(defn/boot "filter" (f seq)
  {:|curry| 1}
  (match-seq-type seq (remove-if-not f (as-list seq))))

(defn/boot "fold" (f init seq)
  {:|curry| 2}
  (match-seq-type seq (reduce f (as-list seq) :initial-value init)))


;;;;;;
;;; I/O stuff
;;;;;;

(defn/boot "print" (x)
  (princ x))

