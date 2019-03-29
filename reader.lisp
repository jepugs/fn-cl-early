(in-package :fn-impl)

;;; hasty copy/paste rewriting of the $ reader macro porting it to fn
(defmacro fn-make-$-function (expr)
  "Like make-$-function, but wraps the expression in fn instead of LAMBDA"
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
    `(|fn|
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
               `(& ,(find -1 $-args :key #'$-arg-number))))
       ;; tell LISP not to complain about unused gensym variables
       ,@(if ignores `((declare (ignore ,@ignores))))
       ,(if (= (length all-0s) 2)
            `(symbol-macrolet ((,(cadr all-0s) ,(car all-0s)))
               ,expr)
            expr))))

(defun fn-$-reader (stream char)
  "Implementation of the $-reader for fn. See macros.lisp."
  (let ((*readtable* (copy-readtable)))
    (let ((c (peek-char nil stream nil)))
      (if (member c *$-dispatch-chars*)
          `(fn-make-$-function ,(read stream))
          (progn
            (set-macro-character #\$ nil)
            (read (make-concatenated-stream
                   (make-string-input-stream (string char))
                   stream)))))))

(defparameter fn-readtable (copy-readtable nil))

(let ((*readtable* fn-readtable))
  (setf (readtable-case *readtable*) :preserve)
  (set-macro-character #\$ #'fn-$-reader t)
  (set-del-reader #\[ #\] '|list|)
  (set-del-reader #\{ #\} '|dict|)
  ;; imagine that: a lisp that doesn't abuse the hash character!
  (set-macro-character #\# nil)
  (set-macro-character #\| nil))

(defun fn-read (&optional (stream *standard-input*))
  (let ((*readtable* fn-readtable))
    (read)))
