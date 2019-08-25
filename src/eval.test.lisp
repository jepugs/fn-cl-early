(in-package :fn.eval)

(use-package :fn.test)
(use-package :fn.boot)

(defun eval-str (str)  
  (eval-ast (car (fn.parser:parse-string str))))
(defun var-value (str)
  (aif (env-cell *current-env* (fnintern str))
       (cell-value it)
       nil))

;; tests in this suite happen in a shared global environment so that name resolution may be checked.
(define-test-suite fn.eval)

;;; atom creation
(define-test (eval-str "17") (num 17))
(define-test (eval-str "-6") (num -6))
(define-test (eval-str "true") true)
(define-test (eval-str "false") false)
(define-test (eval-str "\"hello\"") "hello")

;;; function calling
(define-test (eval-str "(String (+ 2 4) \" \" 3)") "6.0 3.0")
(define-test (eval-str "(- (+ 2 4) 3)") (num 3))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; special operator tests

;; apply
(define-test (eval-str "(apply + [1 2 3])") (num 6))
(define-test (eval-str "(apply + 1 [1 2 3])") (num 7))
(define-error-test (eval-str "(apply + 1)")
    :message "Runtime error: apply: Last argument must be a list")

;; class-of on built-in types
(define-test (eval-str "(class-of (class-of 6))") (var-value "Class"))
(define-test (eval-str "(class-of 6)") (var-value "Num"))
(define-test (eval-str "(class-of \"test\")") (var-value "String"))
(define-test (eval-str "(class-of [])") (var-value "List"))
(define-test (eval-str "(class-of [1])") (var-value "List"))
;; TODO: module, method, general object class test

;; cond
(define-test (eval-str "(cond true 1)") (num 1))
(define-test (eval-str "(cond false 1)") fnnull)
(define-test (eval-str "(cond false 1 true 2 true 3)") (num 2))

;; function and variable def
(define-test (eval-str "(def x 2)") (num 2))
(define-test (eval-str "x") (num 2))
(define-test (eval-str "(def (f x) (* x x))") t :test #'values)
(define-test (eval-str "(f x)") (num 4))
(define-test (eval-str "(def (g x) (if (= x 3) \"x is 3!\" null))") t :test #'values)
(define-test (eval-str "(g 3)") "x is 3!")
(define-test (eval-str "(g 4)") fnnull)

;; defclass & object creation
(define-test (eval-str "(defclass (Vec2 x y))") nil
  :test $(fnclass? $1))
(define-error-test (eval-str "(Vec2 1 -1)") :no-error t)

;; defmethod, method definition, and method calls
(define-test (eval-str "(defmethod ((m obj) obj))") nil
  :test $(fnmethod? $1))
(define-error-test (eval-str "(def ((m Num) x) (String \"(num \" x \")\"))")
    :no-error t)
(define-test (eval-str "(m 2)") "(num 2.0)")
(define-error-test (eval-str "(m \"str\")")
    :message "Runtime error: m: Method not implemented on types (String)."
    )

(defvar q)

;; defvar and instantiation
(define-error-test (eval-str "(defvar v (Vec2 1 -1))") :no-error t)
(define-test (eval-str "(= v (Vec2 1 -1))") true)

;; get
(define-test (eval-str "(get (Vec2 1 -1) 'x)") (num 1))
(define-test (eval-str "v.y") (num -1))
;;(define-test (eval-str "v.z") (num -1))

;; set
(define-error-test (eval-str "(set v (Vec2 0 0))") :no-error t)
(define-test (eval-str "v.x") (num 0))
(define-test (eval-str "v.y") (num 0))
(define-error-test (eval-str "(set v.x 4)") :no-error t)
(define-test (eval-str "v.x") (num 4))


;; fresh runtime for running
(let* ((*runtime* (init-runtime))
       (*current-env* (init-env)))
  (run-tests))
