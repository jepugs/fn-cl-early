
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
(define-error-test (validate-code (str->code "(apply 2)"))
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

;; test dotted gets
(define-test (code-dotted-get? (str->code "x.y")) t)
(define-test (code-dotted-get? (str->code "x.y.z")) t)
(define-test (code-dotted-get? (str->code "x.y.z.w")) t)
(define-test (code-dotted-get? (str->code "(get x 'y)")) t)
(define-test (code-dotted-get? (str->code "(get x y)")) nil)
(define-test (code-dotted-get? (str->code "(get x.y 'z)")) t)
(define-test (code-dotted-get? (str->code "(get x.y z)")) nil)

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

;; import
(define-error-test (validate-code (str->code "(import)"))
    :message "too few arguments for import")
(define-error-test (validate-code (str->code "(import seq)"))
    :no-error t)
(define-error-test (validate-code (str->code "(import seq 'as)"))
    :message "wrong number of arguments for import")
(define-error-test (validate-code (str->code "(import seq 'as 2)"))
    :message "import: as value must be a symbol")
(define-error-test (validate-code (str->code "(import seq 'as 'y)"))
    :message "import: as value must be a symbol")
(define-error-test (validate-code (str->code "(import seq 'as x)"))
    :no-error t)
(define-error-test (validate-code (str->code "(import seq 'nas x)"))
    :message "illegal arguments to import")

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

;; set
(define-error-test (validate-code (str->code "(set x)"))
    :message "Wrong number of arguments for set")
(define-error-test (validate-code (str->code "(set 2 3)"))
    :message "Malformed place in set")
(define-error-test (validate-code (str->code "(set (get x 2) 3)"))
    :no-error t)

(run-tests)

