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
  (:use :cl :fn.scanner :fn.ast :fn.parser :fn.util :fn.values :fn.eval)
  (:export :main))

(in-package :fn.main)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun main ()
  (let ((args sb-ext:*posix-argv*))
    ;; check arguments
    (when (or (some $(or (equal "-h" $)
                         (equal "--help" $))
                    args)
              (not (length< args 3)))
      (show-usage)
      (sb-ext:exit :code -1))
    (if (cdr args)
        (with-open-file (in (cadr args) :direction :input) 
          (handler-case (mapcar $(eval-ast $)
                                (parse (scan in)))
            (fn-error (x)
              (princ x)
              (terpri))))
        (loop (handler-case
                  (progn
                    (princ "> ")
                    (finish-output)

                    (mapcar $(fnprintln-code (eval-ast $))
                            (parse (scan-from-string (read-line)))))

                ;; exit on SIGINT
                (sb-int::interactive-interrupt (x)
                  (princ x)
                  (terpri)
                  (sb-ext:exit :code -1))

                ;; exit on end of file
                (end-of-file ()
                  (terpri)
                  (sb-ext:exit :code 0))

                ;; print out and continue on fn-error
                (fn-error (x)
                  (princ x)
                  (terpri)))))))

(defun show-usage ()
  (format t "Usage: fn [script file]~%"))
