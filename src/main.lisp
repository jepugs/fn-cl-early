;;;; main.lisp -- main executable

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

(defpackage :fn.main
  (:documentation "Main routine for fn")
  (:use :cl :fn.scanner :fn.ast :fn.parser :fn.util :fn.values :fn.runtime :fn.eval :fn.boot)
  (:export :main))

(in-package :fn.main)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun fnprintln (obj)
  (princ (show obj))
  (terpri))

(defun run-repl (&optional (save-globals t))
  "Run fn's Read-Eval-Print Loop. The REPL can be exited by EOF or by sending SIGINT to the
 interpreter process."
  (let* ((*runtime* (init-runtime))
         (*current-env* (init-env)))
    (loop (handler-case
              (progn
                (princ "> ")
                (finish-output)
                (mapcar $(fnprintln (eval-ast $))
                        (parse (scan-from-string (read-line)))))

            ;; quit REPL on SIGINT
            (sb-int::interactive-interrupt (x)
              (princ x)
              (terpri)
              (return-from run-repl))

            ;; quit REPL on end of file
            (end-of-file ()
              (terpri)
              (return-from run-repl))

            ;; print out and continue on fn-error
            (fn-error (x)
              (princ x)
              (terpri))))))

(defun main ()
  (let ((args sb-ext:*posix-argv*))
    ;; check arguments
    (when (or (some $(or (equal "-h" $)
                         (equal "--help" $))
                    args)
              (not (length< args 3)))
      (show-usage)
      (sb-ext:exit :code -1))
    (setf *runtime* (init-runtime))
    (setf *current-env* (init-env))
    (let* ((*runtime* (init-runtime))
           (*current-env* (init-env)))
      (if (cdr args)
          (eval-file (cadr args))
          (progn (run-repl nil)
                 (sb-ext:exit))))))

(defun show-usage ()
  (format t "Usage: fn [script file]~%"))
