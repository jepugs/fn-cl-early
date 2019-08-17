(in-package :fn.eval)

(use-package :fn.test)

(defun eval-str (str)  
  (eval-ast (car (fn.parser:parse-string str))))

;; tests in this suite happen in a shared global environment so that name resolution may be checked.
(define-test-suite fn.eval)

(define-test (eval-str "(def x 2)") (num 2))
(define-test (eval-str "x") (num 2))
(define-test (eval-str "(def (f x) (* x x))") t :test #'values)
(define-test (eval-str "(f x)") (num 4))
(define-test (eval-str "(def (g x) (if (= x 3) \"x is 3!\" null))") t :test #'values)
(define-test (eval-str "(g 3)") "x is 3!")
(define-test (eval-str "(g 4)") fnnull)


(let* ((*interpreter* (init-interpreter))
       (*current-env* (init-env nil nil (car (interpreter-modules *interpreter*)))))
  (run-tests))
