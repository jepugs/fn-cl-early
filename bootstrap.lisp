;;;; boostrap.lisp -- set up the fn language environment
(in-package :fn-impl)

(defpackage :|fn|
  (:documentation "Built-in definitions for the fn programming language."))

(defvar prev-readtable nil
  "The readtable before we change it.")
(defvar prev-package nil
  "The package before we change it.")

(defmacro boot-fn ()
  "Set up all the fn environment. This allows you to use fn directly in your CL
 repl."
  `(progn
     (setq prev-readtable *readtable*)
     (setq prev-package *package*)
     ,@(mapcar (lambda (x)
                 (let* ((old-sym (if (listp x) (car x) x))
                        (new-name (if (listp x) (cadr x) (symbol-name x))))
                   `(|def| ,(intern new-name :|fn|) ,old-sym)))
               bootlib-lexical-exports)
     ,@(mapcar (lambda (x)
                 (let* ((old-sym (if (listp x) (car x) x))
                        (new-name (if (listp x) (cadr x) (symbol-name x)))
                        (args (gensym)))
                   `(cl:defmacro ,(intern new-name :|fn|) (&body ,args)
                      `(,',old-sym ,@,args))))
               bootlib-macro-exports)
     ,@(mapcar (lambda (x)
                 (let* ((old-sym (if (listp x) (car x) x))
                        (new-name (if (listp x) (cadr x) (symbol-name x))))
                   `(cl:define-symbol-macro ,(intern new-name :|fn|) ,old-sym)))
               bootlib-sym-macro-exports)
     (setq *readtable* fn-readtable)
     (setq *package* (find-package :|fn|))))

(defmacro unboot-fn ()
  "Restore the Common Lisp environment from before"
  `(progn (setq *readtable* prev-readtable)
          (setq *package* prev-package)))

(defmacro |unboot-fn| ()
  "Lowercase version to make it easier to type from fn"
  '(unboot-fn))
