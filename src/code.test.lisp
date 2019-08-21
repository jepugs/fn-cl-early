
(in-package :fn.code)

;; add the testing package
(use-package :fn.test)

(defvar *test-symtab* (make-instance 'symtab))

(defun str->code (str)
  (ast->code (car (fn.parser::parse-string str))
             (make-symtab)))

(define-test-suite test-fn.code)

;;;; make sure that validation errors work

;; argument length checking
(define-error-test (validate-code (str->code "(apply test)"))
    :message "too few arguments for apply")
(define-error-test (validate-code (str->code "(quote)"))
    :message "wrong number of arguments for quote")
(define-error-test (validate-code (str->code "(quote 1 2)"))
    :message "wrong number of arguments for quote")

;;; apply
(define-error-test (eval-str "(apply 2)")
    :message "too few arguments for apply")

;; cond
(define-error-test (validate-code (str->code "(cond)"))
    :message "too few arguments for cond")
(define-error-test (validate-code (str->code "(cond true false 3)"))
    :message "odd number of arguments to cond")

;; def
(define-error-test (validate-code (str->code "(def x)"))
    :message "too few arguments for def")
(define-error-test (validate-code (str->code "(def x 14 15)"))
    :message "too many arguments in definition of x")
(define-error-test (validate-code (str->code "(def (x 17) 18)"))
    :message "in function definition parameters: malformed parameter")
(define-error-test (validate-code (str->code "(def ((x) 17) 18)"))
    :message "in method definition parameters: malformed parameter")

(define-error-test (validate-code (str->code "(defvar x)"))
    :message "wrong number of arguments for defvar")

;; let
(define-error-test (validate-code (str->code "(let ())"))
    :message "too few arguments for let")
(define-error-test (validate-code (str->code "(let \"str\" expr)"))
    :message "let bindings must be a list")
(define-error-test (validate-code (str->code "(let (x) expr)"))
    :message "odd number of items in let binding list")
(define-error-test (validate-code (str->code "(let (x y z) expr)"))
    :message "odd number of items in let binding list")
(define-error-test (validate-code (str->code "(let (1 2) expr)"))
    :message "let vars must be symbols")

;; quasiquote
(define-error-test (validate-code (str->code "`,2")) :no-error t)
(define-error-test (validate-code (str->code "`,,2"))
    :message "unquote outside of quasiquote")
(define-error-test (validate-code (str->code "``,,2")) :no-error t)

;; unquoting
(define-error-test (validate-code (str->code ",test"))
    :message "unquote outside of quasiquote")
(define-error-test (validate-code (str->code ",@test"))
    :message "unquote-splice outside of quasiquote")

(run-tests)

