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
  (:export :define-test-suite :define-test :define-test/eq :define-error-test :run-tests))

(in-package :fn.test)

(defvar *current-test-suite* nil
  "The TEST-SUITE used by DEFINE-TEST.")

(defstruct test-suite
  name
  tests)

(defstruct unit-test
  (expr nil :read-only t)
  (thunk nil :type function :read-only t)
  ;; function used to check result of the thunk
  (test-fun #'equal :type function :read-only t))

(defun run-test (unit-test)
  "Run a test and write results to *STANDARD-OUTPUT*"
  (with-slots (expr thunk test-fun) unit-test
    (handler-case 
        (let ((res (funcall thunk)))
          (if (funcall test-fun res)
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

(defmacro define-test (expression &optional test-fun)
  "Define a unit test and add it to *CURRENT-TEST-SUITE*. EXPRESSION is the code to evaluate when
 the test is run. TEST-FUN is a function that is called on the result to determine whether the test
 passed. A NIL return value indicates failure and anything else indicate success.

 If TEST-FUN is not provided, then the test will succeed unless it causes an error.

 The current test suite may be run with (RUN-TESTS)."
  `(push (make-unit-test :expr ',expression
                         :thunk (lambda () ,expression)
                         :test-fun ,(if test-fun
                                        test-fun
                                        (lambda (x)
                                          (declare (ignore x))
                                          t)))
         (test-suite-tests *current-test-suite*)))

(defmacro define-test/eq (expression expected-result)
  "Define a unit test that checks whether the result EXPRESSION is equal to that of EXPECTED-RESULT 
 using EQUALP.

 Neither EXPRESSION nor EXPECTED-RESULT is evaluated until the test is executed, so the code in
 EXPECTED-RESULT may rely on side-effects caused by EXPRESSION."
  (with-gensyms (res)
    `(push (make-unit-test :expr ',expression
                           :thunk (lambda () ,expression)
                           :test-fun (lambda (,res)
                                       (equalp ,res ,expected-result)))
           (test-suite-tests *current-test-suite*))))

(defun verify-error (thunk &key message (num-lines 1) origin no-error)
  "Perform a unit test on an expression that should throw an FN-ERROR. If MESSAGE or ORIGIN are
 supplied, the respective slots in the error must be EQUAL to them. If neither is specified the test
 will pass on an FN-ERROR regardless of its slot values. If no error is emitted, the test will
 always fail. Only the first NUM-LINES lines of the message are checked. NUM-LINES has a default
 value of 1 so stack traces from runtime errors are ignored. Setting it to a non-numeric value
 allows "
  (handler-case (progn (funcall thunk)
                       no-error)
    ;; get the fn error and check its slots
    (fn-error (x)
      (let* ((actual-message (slot-value x 'message)) 
             (message-test (if message
                               (if (numberp num-lines)
                                   (equalp (take num-lines (lines message))
                                           (take num-lines (lines actual-message)))
                                   (string= actual-message message))
                               t)))
        (and (not no-error)
             message-test
             (if origin
                 (equal (slot-value x 'origin) origin)
                 t))))))

(defmacro define-error-test (expression &key message (num-lines 1) origin no-error)
  "Add an error test to the current test suite"
  `(define-test
       (verify-error (lambda () ,expression)
                     ,@(if message `(:message ,message) nil)
                     ,@(if origin `(:origin ,origin) nil)
                     ,@(if num-lines `(:num-lines ,num-lines))
                     :no-error ,no-error)
       #'values))

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

