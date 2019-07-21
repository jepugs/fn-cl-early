;;;; core.lisp -- interpreter state

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


(defclass fn-symb ()
  ((name :initarg :name
         :type string
         :documentation "Symbol name")
   (binding :initarg :binding
            :type symbol
            :documentation "The sort of variable binding. One of :IMMUTABLE, :MUTABLE, or NULL")
   (macro :initarg :macro
          :documentation "The value bound to this symbol's macro slot")
   (var :initarg :var
        :documentation "The value bound to this symbol's variable slot"))
  (:documentation "A symbol"))

(defclass module ()
  ((name :initarg :name
         :type keyword
         :documentation "The name of this module.")
   (exports :initarg :exports
            :type list
            :documentation "List of symbols exported by this module.")
   (symb-tab :initarg :sym-tab
             :type hash-table
             :documentation "Hash table used to look up symbols by name"))
  (:documentation "A module contains a bunch of strings."))

(defclass core ()
  ((modules :initarg :modules
            :documentation "Hash table of all modules known by this core.")
   (threads :initarg :threads
            :documentation "List of threads of the interpreter.")
   (main-thread :initarg :main-thread
                :documentation "The main thread of the interpreter."))
  (:documentation "Represents an interpreter state. This holds all variables, macros, etc for the
 interpreter."))

(defclass environment ()
  ((lexical :initarg :lexical
            :type list
            :documentation "A stack of hash tables constituting the current lexical environment.")
   (module :initarg :module
           :type module
           :documentation "The module used for name resolution."))
  (:documentation "Environment for name resolution."))

(defclass thread ()
  ((call-stack :initarg :call-stack
               :documentation "Call stack containing debugging info.")
   (continue :initarg :continue
        :documentation "Function used to continue this thread's evaluation. Takes one argument,
 which should be this thread. This will mutate the thread.")
   (core :initarg :core
         :initform global-core
         :documentation "The core to which this thread belongs.")
   (env :initarg :env
        :type environment
        :documentation "Stack of hash tables containing the current lexical environment.")
   (module :initarg :module
           :documentation "The module used for name resolution")
   (state :initarg :state
          :initform :pause
          :documentation "One of :RUNNING, :STOPPED, :WAITING, or :DEAD"))
  (:documentation "The state of a single thread of the interpreter."))

(defvar global-core nil
  "Singleton interpreter core.")

(defun init-modules ()
  "Create a modules hash table containing just one empty module named user."
  (make-eq-ht :|user|
              (make-instance 'module
                             :name :|user|
                             :exports nil
                             :var (make-eq-ht)
                             :macro (make-eq-ht))))

(defun init-main-thread (core continue)
  (make-instance 'thread
                 :call-stack nil
                 :continue continue
                 :env nil
                 :module (gethash :|user| modules)
                 :state :stopped))

(defun init-core (continue)
  "Create an empty core using the function continue"
  (let* ((modules (init-modules))
         (main-thread (init-main-thread (gethash :|user| modules) continue)))
    (make-instance 'core
                   :modules modules
                   :threads (list main-thread)
                   :main-thread main-thread)))

(defun start-thread (th)
  "Function used to start a stopped thread. This does not actually cause a new thread be started, so
 that should be handled before this function is called."
  (setf (slot-value th 'state) :running)
  (run-thread th))

(defun run-thread (th)
  "The run loop for a thread."
  (with-slots (continue state) th
    (when (eq state :running)
      (funcall continue th)
      (run-thread th))))

(defun find-var (symbol module env)
  "Look up a variable in the given module and lexical environment"
  (if env
      (multiple-value-bind (x found) (gethash symbol (car env))
        (if found
            x
            (find-var symbol module (cdr env))))
      (multiple-value-bind (x found) (gethash symbol (slot-value module 'var))
        (if found
            x
            (error "FIND-VAR: symbol ~s not found" symbol)))))

(defun find-macro (symbol module env)
  "Look up a macro in the given module and lexical environment"
  (gethash symbol (slot-value module 'macro)))

(defun add-module-var (module symbol value)
  "Add a variable to the module. Raises an error if the variable already exists and is immutable."
  ())

(defun add-module-macro (module symbol macro)
  "Add a macro to the module."
  ())
