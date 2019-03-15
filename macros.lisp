;;;; macros.lisp --- A conservative set of FN reader and control flow macros
;;;; implemented in portable ANSI Common Lisp.

(in-package :fn-impl)

;;;;;;
;;; Utility functions
;;;;;;

(defmacro aif (test then else)
  "Anaphoric IF binds the value of TEST to IT in the clauses."
  `(let ((it ,test))
     (if it ,then ,else)))

(defun flatten (tree)
  "Turn a tree into a list of its leaves. Returns a list even if the tree is an
 atom, e.g. (flatten 'a) -> (A)."
  (cond ((listp tree) (mapcan #'flatten tree))
        #+sbcl
        ((sb-impl::comma-p tree) (flatten (sb-impl::comma-expr tree)))
        (t (list tree))))

(defun symb (&rest args)
  "Make a symbol by concatenating ARGS as a string"
  (let ((strs (mapcar #'princ-to-string args)))
    (intern (apply #'concatenate 'string strs))))

(defun group (n list)
  "Partition LIST into sublists of length N"
  (labels ((recur (m tail res)
             (if (zerop m)
                 (cons (nreverse res) (group n tail))
                 (recur (1- m) (cdr tail) (cons (car tail) res)))))
    (if list
        (recur n list nil)
        nil)))

(defun name-eq (sym1 sym2)
  "Check symbol-name equality"
  (string= (symbol-name sym1) (symbol-name sym2)))

(defun macroexpand-all (tree)
  "Recursively macroexpand a form before evaluation. This changes macro
 semantics slightly, (particularly involving special forms like DECLARE), but
 we're lighting fires here anyway."
  (if (listp tree)
      (let ((tree0 (macroexpand tree)))
        (if (listp tree)
            (cons (car tree0)
                  (mapcar #'macroexpand-all (cdr tree0)))
            tree))
      tree))

(defun is-quoted (expr)
  "Tell if EXPR is a quoted expression."
  (and (listp expr)
       (eq (car expr) 'quote)
       (= (length expr) 2)))

(defmacro rlambda (llist init-list &body body)
  "Creates a recursive lambda form and calls it with init-list"
  `(labels ((recur ,llist
              ,@body))
     (recur ,@init-list)))

(defun take (n lst)
  "Take the first n entries from a list"
  (loop for x in lst
     for i from 1 to n
     collect x))

(defun drop (n lst)
  (if (zerop n)
      lst
      (drop (- n 1) (cdr lst))))

(defun take-while (test lst)
  "Take entries from lst until test returns false."
  (loop for x in lst
     while (funcall test x)
     collect x))

(defun split-if (test lst)
  "Split a list at the first element that satisfies test. Returns [left right]."
  (rlambda (rleft right) (nil lst)

    ;; ;; this is what this function could look like in fn <3
    ;; (case right
    ;;   [(satisfies _ test) & _]          ;=>
    ;;     [(nreverse rleft) right]
    ;;   [a & b]                           ;=>
    ;;     (recur (cons a rleft) b)
    ;;   []                                ;=>
    ;;     [(nreverse rleft) right])

    (cond ((null right)
           (list (nreverse rleft) right))
          ((funcall test (car right))
           (list (nreverse rleft) right))
          (t
           (recur (cons (car right) rleft) (cdr right))))))

(defun interleave (&rest lst*)
  (rlambda (res l*) (() lst*)
    ;; FIXME: this could be made more efficient by using a vector for l* and
    ;; keeping track of the index mod (length l*) instead of rotating the list
    ;; with each recursive call
    (cond ((null l*) (nreverse res))
          ;; remove empty lists
          ((null (car l*)) (recur res (cdr l*)))
          ;; add non-empty element and cycle l*
          (t (recur (cons (caar l*) res) (append (cdr l*) (list (cdar l*))))))))


;;;;;;
;;; Dollar-sign reader macro
;;;;;;

(declaim (inline |$-lambda|))
(defun |$-lambda| (form)
  "A stand-in function used to do nested $()."
  form)
(define-compiler-macro |$-lambda| (form)
  form)

(defun dollar-flatten (tree)
  "Like FLATTEN, but discards |$-lambda$| forms and quoted symbols."
  (cond
    #+sbcl
    ((sb-impl::comma-p tree) (flatten (sb-impl::comma-expr tree)))
    ((not (listp tree)) (list tree))
    ((is-quoted tree) nil)
    ;; removing $-lambda forms lets us 
    ((eq (car tree) '|$-lambda|) nil)
    ;; FIXME: we can improve behavior by accounting for variable-binding
    ;; forms like LET, but also, who really cares? (not me -- Jack)
    (t (mapcan #'dollar-flatten tree))))

(defun is-string-numid (str)
  "Tell if STR is a positive integer without leading zeros (i.e. a numerical id)."
  (and (not (char= (aref str 0) #\0))
       (every #'digit-char-p str)))

(defun is-$-arg (sym)
  "Tell if SYM is a valid $-arg for the $ reader macro."
  (and (symbolp sym)
       (or (name-eq sym '$)
           (name-eq sym '$0)
           (name-eq sym '$&)
           (and (char= (aref (symbol-name sym) 0) #\$)
                (is-string-numid (subseq (symbol-name sym) 1))))))

(defun $-arg-number (sym)
  "Get the numerical id from a $-arg."
  (cond ((name-eq sym '$&) -1)
        ((name-eq sym '$) 0)
        (t (parse-integer (subseq (symbol-name sym) 1)))))

(defmacro make-$-function (expr)
  "Find $-args and wrap EXPR in an appropriate LAMBDA form."
  ;; FIXME: (maybe) we don't account for the possibility that EXPR contains
  ;; $-args from multiple different packages. This could lead to esoteric
  ;; variable binding errors.
  (let* (($-args (remove-duplicates
                  (remove-if-not #'is-$-arg (dollar-flatten expr))))
         (argc (apply #'max -1 (mapcar #'$-arg-number $-args)))
         (ignores nil)
         ;; if both $0 and $ are used, then this list will have length 2
         (all-0s (remove-if-not (lambda (x)
                                  (= ($-arg-number x) 0))
                                $-args)))
    `(lambda
         ;; create the argument list
         ,(append
           (loop for i from 0 to argc
              ;; $-reader is package agnostic, so we have to use the symbols we
              ;; found in the function body instead of making them fresh
              for sym = (find i $-args :key #'$-arg-number)
              collect (if sym
                          sym
                          ;; for unused numeric arguments, put in a gensym
                          (let ((g (gensym)))
                            (push g ignores)
                            g)))
           ;; add the &REST to variadic functions
           (if (member '$& $-args :test #'name-eq)
               `(&rest ,(find -1 $-args :key #'$-arg-number))))
       ;; tell LISP not to complain about unused gensym variables
       ,@(if ignores `((declare (ignore ,@ignores))))
       ,(if (= (length all-0s) 2)
            `(symbol-macrolet ((,(cadr all-0s) ,(car all-0s)))
               ,expr)
            expr))))

(defparameter *$-dispatch-chars* '(#\( #\[ #\{ #\`)
  "A list of characters that trigger the $ reader macro.")

(defun $-reader (stream char)
  "Read in an anonymous function. $(expr) expands to a LAMBDA form. The
arguments to the function are accessed using $-args (which are precisely
described below). The generated functions take exactly as many arguments as the
highest numbered arguments, but can accept variadic arguments if the symbol $&
occurs in expr.

EXAMPLES:
> ;; add 17 to a list
> (mapcar $(+ $ 17) '(1 2 3 4))
 (18 19 20 21)
> ;; sum even numbers at each index
> (mapcar $(apply #'+ (remove-if #'oddp $&)) '(-1 5 2) '(2 4 0) '(-2 -3 -1))
 (0 4 2)
> ;; convert a list of bits to a number
> (reduce $(+ (* $0 2) $1) '(1 0 1 1))
 11

TYPES OF $-ARGS:
 $  is the first argument (equivalent to $0).
 $N where N is a natural number with no leading 0s, accesses the Nth
    argument (indexed by 0). E.g. $1 gets the second argument, $2 the third,
    and so on.
 $& gets all arguments after the highest numeric $-arg

$ DISPATCH CHARACTERS:
  The $ reader macro is nonterminating and is only triggered when it is followed
by one of these characters: (, [, {, or `. Otherwise, the $-reader is
temporarily disabled and the stream is read normally.

NESTING $ EXPRESSIONS:
  $ expressions can be trivially nested. $-args in the higher levels are
completely shadowed in the lower levels. This is a decision made to keep syntax
simple and to discourage abuse of $().

QUIRKS:
  $-args are detected based on symbol name and are indifferent to package. The
surrounding lambda binds these names directly as function arguments. In
detecting $-args, the expression is macroexpanded first, and non-lists, quoted
forms, and nested $() forms are discarded, but no non-trivial code walking is
done other than that. Thus, you should avoid using variables with $-arg names in
the function body, as it could mess up the argument list.
  The ->$ macro can safely be used within $() because the $ symbol is eliminated
from the body during macroexpansion. More exotic macros may have their behavior
broken by the messed up macroexpansion order.
"
  (let ((*readtable* (copy-readtable)))
    (let ((c (peek-char nil stream nil)))
      (if (member c *$-dispatch-chars*)
          `(make-$-function ,(read stream))
          (progn
            (set-macro-character #\$ nil)
            (read (make-concatenated-stream
                   (make-string-input-stream (string char))
                   stream)))))))


;;;; M A K E I T S O
;; ^^ I don't like star trek, but I like that quote and it's actually a historic
;; British navy phrase so they didn't even invent it.
(eval-when (:load-toplevel)
  (set-macro-character #\$ #'$-reader t))

;;; BIGFIXME: fuck I forgot but i pretty sure it's in the reader function


;;;;;;
;;; threading/pipeline macros
;;;;;;

;;; Note: These look much nicer using $() syntax

(defmacro -> (form &body lists)
  (reduce (lambda (inner outer)
            `(,(car outer) ,inner ,@(cdr outer)))
          ;; Wrap individual symbols in lists so we can provide operator names
          ;; to our pipelines. E.g. log -> (log).
          ;; FIXME: this behavior is weird. Maybe get rid of it?
          (mapcar (lambda (x)
                    (if (symbolp x)
                        (list x)
                        x))
                  lists)
          :initial-value form))

(defmacro ->> (form &body lists)
  (reduce (lambda (inner outer)
            (append outer (list inner)))
          (mapcar (lambda (x)
                    (if (symbolp x)
                        (list x)
                        x))
                  lists)
          :initial-value form))

(defmacro ->$ (form &body lists)
  (reduce (lambda (inner outer)
            (substitute-if
             outer
             (lambda (x)
               (and (symbolp x)
                    (name-eq x '$)))
             inner))
          lists
          :initial-value form))

(defmacro as-> (form var &body lists)
  (reduce (lambda (inner outer)
            (substitute-if
             outer
             (lambda (x) (eq x var))
             inner))
          lists
          :initial-value form))


;;;;;;
;;; delimiter readers
;;;;;;

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
  (set-del-reader #\{ #\} 'dict))

