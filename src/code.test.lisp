
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
    :message "apply: Too few arguments")
(define-error-test (validate-code (str->code "(quote)"))
    :message "quote: Wrong number of arguments")
(define-error-test (validate-code (str->code "(quote 1 2)"))
    :message "quote: Wrong number of arguments")

;;; apply
(define-error-test (validate-code (str->code "(apply 2)"))
    :message "apply: Too few arguments")

;; cond
(define-error-test (validate-code (str->code "(cond)"))
    :message "cond: Too few arguments")
(define-error-test (validate-code (str->code "(cond true false 3)"))
    :message "cond: Odd number of arguments")

;; def
(define-error-test (validate-code (str->code "(def x)"))
    :message "def: Too few arguments")
(define-error-test (validate-code (str->code "(def x 14 15)"))
    :message "def: Too many arguments in definition of x")
(define-error-test (validate-code (str->code "(def (x 17) 18)"))
    :message "def: malformed parameter")
(define-error-test (validate-code (str->code "(def ((x) 17) 18)"))
    :message "def: malformed parameter")

;; test dotted gets
(define-test/eq (code-dotted-get? (str->code "x.y")) t)
(define-test/eq (code-dotted-get? (str->code "x.y.z")) t)
(define-test/eq (code-dotted-get? (str->code "x.y.z.w")) t)
(define-test/eq (code-dotted-get? (str->code "(get x 'y)")) t)
(define-test/eq (code-dotted-get? (str->code "(get x y)")) nil)
(define-test/eq (code-dotted-get? (str->code "(get x.y 'z)")) t)
(define-test/eq (code-dotted-get? (str->code "(get x.y z)")) nil)

(define-error-test (validate-code (str->code "(defvar x)"))
    :message "defvar: Wrong number of arguments")

;; let
(define-error-test (validate-code (str->code "(let ())"))
    :message "let: Too few arguments")
(define-error-test (validate-code (str->code "(let \"str\" expr)"))
    :message "let: Bindings must be a list")
(define-error-test (validate-code (str->code "(let (x) expr)"))
    :message "let: Odd number of items in binding list")
(define-error-test (validate-code (str->code "(let (x y z) expr)"))
    :message "let: Odd number of items in binding list")
(define-error-test (validate-code (str->code "(let (1 2) expr)"))
    :message "let: Variable names must be symbols")

;; import
(define-error-test (validate-code (str->code "(import)"))
    :message "import: Too few arguments")
(define-error-test (validate-code (str->code "(import seq)"))
    :no-error t)
(define-error-test (validate-code (str->code "(import seq 'as)"))
    :message "import: Wrong number of arguments")
(define-error-test (validate-code (str->code "(import seq 'as 2)"))
    :message "import: 'as value must be a symbol")
(define-error-test (validate-code (str->code "(import seq 'as 'y)"))
    :message "import: 'as value must be a symbol")
(define-error-test (validate-code (str->code "(import seq 'as x)"))
    :no-error t)
(define-error-test (validate-code (str->code "(import seq 'nas x)"))
    :message "import: illegal arguments")

;; quasiquote
(define-test (validate-code (str->code "`,2")))
(define-error-test (validate-code (str->code "`,,2"))
    :message "unquote: unquote outside of quasiquote")
(define-test (validate-code (str->code "``,,2")))

;; unquoting
(define-error-test (validate-code (str->code ",test"))
    :message "unquote: unquote outside of quasiquote")
(define-error-test (validate-code (str->code ",@test"))
    :message "unquote-splice: unquote-splice outside of quasiquote")

;; set
(define-error-test (validate-code (str->code "(set x)"))
    :message "set: Wrong number of arguments")
(define-error-test (validate-code (str->code "(set 2 3)"))
    :message "set: Malformed place")
(define-test (validate-code (str->code "(set (get x 2) 3)")))

(run-tests)

