;;;; test.lisp -- unit testing framework for fn

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

(defpackage :fn.test
  (:documentation "unit testing framework for fn")
  (:use :cl :fn.util)
  (:export :define-test-suite :define-test :define-error-test :run-tests))

(in-package :fn.test)

(defvar *current-test-suite* nil
  "The TEST-SUITE used by DEFINE-TEST.")

(defstruct test-suite
  name
  tests)

(defstruct unit-test
  (expr nil :read-only t)
  (thunk nil :type function :read-only t)
  (result-thunk nil :read-only t)
  ;; this test is used to compare the result of the thunk with the
  (test #'equal :type function :read-only t))

(defun run-test (unit-test)
  (with-slots (expr thunk result-thunk test) unit-test
    (handler-case 
        (let ((res (funcall thunk)))
          (if (funcall test (funcall result-thunk) res)
              (progn (format t "[pass] ~s~%" expr)
                     t)
              (progn (format t "[FAIL] ~s~%" expr)
                     nil)))
      (fn-error () (format t "[FAIL] ~s~%" expr)))))

(defmacro define-test-suite (name)
  (unless (symbolp name)
    (error "DEFINE-TEST-SUITE: NAME must be a symbol"))
  `(progn
     (defparameter ,name (make-test-suite :name ',name))
     (setq *current-test-suite* ,name)))

(defmacro define-test (expression expected-value &key (test #'equal) (suite *current-test-suite*))
  "Define a unit test and add it to SUITE. EXPRESSION should be a Common Lisp expression to evaluate
 and EXPECTED-VALUE is the output indicating a successful test. TEST is the function used to check
 this. Setting TEST to #'VALUES will cause the test to pass unless an FN-ERROR is thrown. Other
 types of error conditions are not handled and will result in the test program crashing."
  `(push (make-unit-test :expr ',expression
                         :thunk (lambda () ,expression)
                         :result-thunk (lambda () ,expected-value)
                         :test ,test)
         (test-suite-tests ,suite)))

(defun verify-error (thunk &key message origin no-error)
  "Perform a unit test on an expression that should throw an FN-ERROR. If MESSAGE or ORIGIN are
 supplied, the respective slots in the error must be EQUAL to them. If neither is specified the test
 will pass on an FN-ERROR regardless of its slot values. If no error is emitted, the test will
 always fail."
  (handler-case (progn (funcall thunk)
                       no-error)
    ;; get the fn error and check its slots
    (fn-error (x)
      (and (not no-error)
           (if message
                 (string= (slot-value x 'message) message)
                 t)
           (if origin
               (equal (slot-value x 'origin) origin)
               t)))))

(defmacro define-error-test (expression &key message origin no-error)
  `(define-test
       (verify-error (lambda () ,expression)
                     ,@(if message `(:message ,message) nil)
                     ,@(if origin `(:origin ,origin) nil)
                     :no-error ,no-error)
       t
     :test #'eq))

(defun run-tests (&optional (test-suite *current-test-suite*))
  (format t
          "------------------------~%TEST SUITE: ~a~%------------------------ ~%"
          (test-suite-name test-suite))
  (let ((succ
         (reduce $(if (run-test $1)
                      (+ $0 1)
                      $0)
                 ;; reverse so that tests appear in the order they were defined
                 (reverse (test-suite-tests test-suite))
                 :initial-value 0))
        (tot (length (test-suite-tests test-suite))))
    (format t "*** RESULTS: ~a/~a passed (~,2f%)~%~%" succ tot (/ (* succ 100.0) tot))))

