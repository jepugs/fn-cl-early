;;;; boostrap.lisp -- set up the fn language environment
(in-package :fn-impl)

(defvar fn-is-booted nil
  "The current state of fn.")
(defvar prev-readtable nil
  "The readtable before we change it.")
(defvar prev-package nil
  "The package before we change it.")

(defmacro boot-fn ()
  "Set up all the fn environment. This allows you to use fn directly in your CL
 repl."
  `(unless fn-is-booted
     (setq prev-readtable *readtable*)
     (setq prev-package *package*)
     (setq fn-is-booted t)
     ,@(reverse bootlib-defs)
     (setq *readtable* fn-readtable)
     (setq *package* (find-package :|fn|))))

(defmacro unboot-fn ()
  "Restore the Common Lisp environment from before"
  `(when fn-is-booted
     (setq *readtable* prev-readtable)
     (setq *package* prev-package)
     (setq fn-is-booted nil)))

(defmacro |unboot-fn| ()
  "Lowercase version to make it easier to type from fn"
  '(unboot-fn))
