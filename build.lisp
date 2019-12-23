;;;; build.lisp -- script to generate fn executable. Must be run from the project root dir

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

(require 'asdf)
;; this is guaranteed to be run from the project directory
(load "fn.asd")

;;;; command line arguments
(defparameter output-dir nil
  "Path to the directory where the compiled executable is placed.")
(defparameter built-in-dir nil
  "Path to the directory containing fn source files to be loaded as part of the built-in module.")
(defparameter stdlib-dir nil
  "Path to the directory containing standard library modules.")

(defun process-args ()
  (let ((args (fn.util:group 2 sb-ext:*posix-argv*)))
    (mapcar $(cond
               ((string= (car $) "--output-dir")
                (setq output-d))
               ((string= (car $) "--built-in-dir")
                (setq built-in-dir (cadr $)))
               ((string= (car $) "--stdlib-dir")
                (setq stdlib-dir nil))))))

(process-args)
(require 'fn)

;;; TODO: load built-in files

;;; TODO: load standard library modules

(sb-ext:save-lisp-and-die (concatenate 'string
                                       output-dir
                                       "/fn")
                          :toplevel #'fn.main:main
                          :executable t
                          :purify t
                          :compression 1)
