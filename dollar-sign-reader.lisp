;;;; dollar-sign-reader.lisp -- dollar sign syntax for terse lambdas

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

(defpackage :fn.dollar-sign-reader
  (:documentation "Dollar sign syntax for terse lambdas")
  (:use :cl :fn.util))

(in-package :fn.dollar-sign-reader)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Dollar-sign reader macro

(declaim (inline |$-lambda|))
(defun |$-lambda| (form)
  "A stand-in function used to do nested $(). This functionality is not currently utilized."
  form)
(define-compiler-macro |$-lambda| (form)
  form)

(defun dollar-flatten (tree &optional (qquote nil))
  "Like FLATTEN, but discards |$-lambda$| forms and quoted symbols. QQUOTE is whether we're within a
 quasiquote form."
  (cond
    #+sbcl                              ;check for commas
    ((sb-impl::comma-p tree) (dollar-flatten (sb-impl::comma-expr tree) nil))
    ((not (listp tree))
     (if qquote
         nil
         (list tree)))
    #+sbcl                              ;turn on quasiquote mode
    ((and (not qquote)
          (eq (car tree) 'sb-int:quasiquote))
     (dollar-flatten (cadr tree) t))
    ;; Normally, quoted values should be ignored. However, within quasiquote, quote forms may
    ;; contain commas that we need to explore recursively
    ((quoted-p tree)
     (if qquote
         (dollar-flatten (cadr tree) t)
         nil))
    ;; removing $-lambda forms lets us nest $
    ((eq (car tree) '|$-lambda|) nil)
    ;; FIXME: I could improve this function by accounting for variable-binding forms like LET, but
    ;; also, who really cares
    (t (mapcan (lambda (x)
                 (dollar-flatten x qquote))
               tree))))

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
  ;; IMPLNOTE: we don't account for the possibility that EXPR contains $-args from multiple
  ;; different packages. This could lead to esoteric variable binding errors. So just don't put
  ;; $-args from multiple packages in there.
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
  "Read in an anonymous function. $(expr) expands to a LAMBDA form. The arguments to the function
are accessed using $-args (which are precisely described below). The generated functions take
exactly as many arguments as the highest numbered arguments, but can accept variadic arguments if
the symbol $& occurs in expr.

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
 $N where N is a natural number with no leading 0s, accesses the Nth argument (indexed by 0). E.g.
    $1 gets the second argument, $2 the third, and so on.
 $& gets all arguments after the highest numeric $-arg

$ DISPATCH CHARACTERS:
  The $ reader macro is nonterminating and is only triggered when it is followed by one of these
characters: (, [, {, or `. Otherwise, the $-reader is temporarily disabled and the stream is read
normally.

NESTING $ EXPRESSIONS:
  $ expressions can be trivially nested. $-args in the higher levels are completely shadowed in the
lower levels. This is a decision made to keep syntax simple and to discourage abuse of $().

QUIRKS:
  $-args are detected based on symbol name and are indifferent to package. The surrounding lambda
binds these names directly as function arguments. In detecting $-args, the expression is
macroexpanded first, and non-lists, quoted forms, and nested $() forms are discarded, but no
non-trivial code walking is done other than that. Thus, you should avoid using variables with $-arg
names in the function body, as it could mess up the argument list. The ->$ macro can safely be used
within $() because the $ symbol is eliminated from the body during macroexpansion. More exotic
macros may have their behavior broken by the messed up macroexpansion order.
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


;;;; finish setup
(eval-when (:load-toplevel)
  (set-macro-character #\$ #'$-reader t))

