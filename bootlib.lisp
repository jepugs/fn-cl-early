;;;; bootlib.lisp -- definitions used to bootstrap the standard library
(in-package :fn-impl)

;;;;;;
;;; Macros for Bootstrap Definitions
;;;;;;

(defparameter bootlib-defs nil
  "Def forms for the fn bootstrap environment.")
(defparameter bootlib-syms nil
  "List of all symbols defined during bootstrapping")

;;; TODO: hook exports code into lexical definitions code to prevent people from
;;; redefining built-in forms.

(defmacro def/boot (name value)
  "Define a lexical global and add it to the lexical export list. This is more
 restricted than the def operator in that it doesn't support pattern matching or
 multiple definitions."
  `(progn
     (push ',(intern name "fn") bootlib-syms)
     (push '(|fn|::|def| ,(intern name "fn") ,value) bootlib-defs)))

(defmacro defn/boot (name arg-list &body body)
  "Define a lexical global function and add it to the lexical export list.
 This macro accepts the same arguments as defn."
  `(progn
     (push ',(intern name "fn") bootlib-syms)
     (push '(|fn|::|defn| ,(intern name "fn") ,arg-list ,@body) bootlib-defs)))

(defmacro defmacro/boot (name arg-list &body body)
  "Creates a macro definition and adds it to the export list. Unlike the
 other /BOOT macros, this uses a Common Lisp-style lambda list."
  `(progn
     (push ',(intern name "fn") bootlib-syms)
     (push '(defmacro ,(intern name "fn") ,arg-list ,@body) bootlib-defs)))

(defmacro def-sym-macro/boot (name expansion)
  `(progn
     (push ',(intern name "fn") bootlib-syms)
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
  `(if (fn-truthy ,test)
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
          $(for-pattern-vars (car $)
                           (cadr $)
                           (lambda (k v)
                             `(define-lexically ,k ,v nil)))
          pairs)
       fn-null)))

(defmacro/boot "def*" (&body pattern-value-pairs)
  "Define a global dynamic variable."
  (let* ((pairs (group 2 pattern-value-pairs)))
    `(progn
       ,@(mapcar $(for-pattern-vars (car $)
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
  (if (fn-truthy x)
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
  (|fn|::|Dict| :|curry| 1)
  (rebool
   (apply #'fn= x0 x)))

(defn/boot "<" (x0 & x)
  (|fn|::|Dict| :|curry| 1)
  (rebool (apply #'< x0 x)))
(defn/boot "<=" (x0 & x)
  (|fn|::|Dict| :|curry| 1)
  (rebool (apply #'<= x0 x)))
(defn/boot ">" (x0 & x)
  (|fn|::|Dict| :|curry| 1)
  (rebool (apply #'> x0 x)))
(defn/boot ">=" (x0 & x)
  (|fn|::|Dict| :|curry| 1)
  (rebool (apply #'>= x0 x)))


;;;;;;
;;; Objects
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
  "Defines a new data type"
  (expand-deftype name arg-list body))

(defn/boot "type-of" (object)
  (slot-value (fn-type-of object) 'name))


(defmacro/boot "defproto" (name &body body)
  (expand-defproto name body))

(defmacro/boot "defimpl" (proto-name type-name &body body)
  (expand-defimpl proto-name type-name body))

;;;;;;
;;; Lists
;;;;;;

(defn/boot "List" (& args)
  (apply #'list args))

(defn/boot "is-list" (x)
  (rebool (listp x)))

(defn/boot "conc" (lst0 & lsts)
  (|fn|::|Dict| :|curry| 1)
  (apply #'append lst0 lsts))

(defn/boot "cons" (hd tl)
  (|fn|::|Dict| :|curry| 1)
  (if (listp tl)
      (cons hd tl)
      (error "cons: tail must be a list")))

(defn/boot "last" (l)
  (car (last l)))



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
  (|fn|::|Dict| :|curry| 1)
  (apply #'+ x0 x))
(defn/boot "-" (x0 & x)
  (|fn|::|Dict| :|curry| 1)
  (apply #'- x0 x))
(defn/boot "*" (x0 & x)
  (|fn|::|Dict| :|curry| 1)
  (apply #'* x0 x))
(defn/boot "/" (x0 & x)
  (|fn|::|Dict| :|curry| 1)
  (apply #'/ x0 x))
(defn/boot "mod" (n d)
  (|fn|::|Dict| :|curry| 1)
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
  (rebool (= (length seq) 0)))

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
  (|fn|::|Dict| :|curry| 1)
  (labels ((iter (acc tl m)
             (|fn|::|if| (|fn|::|and| (|fn|::|not| (|fn|::|is-empty| seq))
                              (|fn|::|<| m n))
                  (iter (cons (car tl) acc) (cdr tl) (+ m 1))
                  [(match-seq-type seq (nreverse acc))
                   (match-seq-type seq tl)])))
    (iter [] seq 0)))

(defn/boot "take" (n seq)
  (|fn|::|Dict| :|curry| 1)
  (subseq seq 0 (min n (length seq))))

(defn/boot "drop" (n seq)
  (|fn|::|Dict| :|curry| 1)
  (subseq seq (min n (length seq))))

(defn/boot "split-when" (f seq)
  (|fn|::|Dict| :|curry| 1)
  (labels ((iter (acc tl)
             (|fn|::|if| (funcall f (car tl))
                  (iter (cons (car tl) acc) (cdr tl))
                  [(match-seq-type seq (nreverse acc))
                   (match-seq-type seq tl)])))
    (iter [] seq)))

(defn/boot "take-while" (f seq)
  (|fn|::|Dict| :|curry| 1)
  (labels ((iter (acc tl)
             (|fn|::|if| (funcall f (car tl))
                  (iter (cons (car tl) acc) (cdr tl))
                  (match-seq-type seq (nreverse acc)))))
    (iter [] seq)))

(defn/boot "drop-while" (f seq)
  (|fn|::|Dict| :|curry| 1)
  (|fn|::|cond|
    (|fn|::|is-empty| seq) seq
    (funcall f (|fn|::|head| seq))
      (|fn|::|drop-while| f (|fn|::|tail| seq))
    fn-true seq))

(defn/boot "map" (f seq)
  (|fn|::|Dict| :|curry| 1)
  (match-seq-type seq (mapcar f (as-list seq))))

(defn/boot "zip" (& seqs)
  (apply #'map 'list #'list (mapcar #'as-list seqs)))

(defn/boot "filter" (f seq)
  (|fn|::|Dict| :|curry| 1)
  (match-seq-type seq (remove-if-not f (as-list seq))))

(defn/boot "fold" (f init seq)
  (|fn|::|Dict| :|curry| 2)
  (match-seq-type seq (reduce f (as-list seq) :initial-value init)))


;;;;;;
;;; I/O stuff
;;;;;;

(defn/boot "print" (x)
  (princ x))

(defn/boot "quit" ()
  (unboot-fn))
