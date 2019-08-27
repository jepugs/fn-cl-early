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
(define-test/eq (eval-str "17") (num 17))
(define-test/eq (eval-str "-6") (num -6))
(define-test/eq (eval-str "true") true)
(define-test/eq (eval-str "false") false)
(define-test/eq (eval-str "\"hello\"") "hello")

;;; function calling
(define-test/eq (eval-str "(String (+ 2 4) \" \" 3)") "6.0 3.0")
(define-test/eq (eval-str "(- (+ 2 4) 3)") (num 3))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; special operator tests

;;; import is not tested here because modules need their own suite of tests

;;; apply
(define-test/eq (eval-str "(apply + [1 2 3])") (num 6))
(define-test/eq (eval-str "(apply + 1 [])") (num 1))
(define-test/eq (eval-str "(apply + 1 [1 2 3])") (num 7))
(define-error-test (eval-str "(apply + 1)")
    :message "Runtime error: apply: Last argument must be a list")

;;; class-of on built-in types
(define-test/eq (eval-str "(class-of (class-of 6))") (var-value "Class"))
(define-test/eq (eval-str "(class-of 6)") (var-value "Num"))
(define-test/eq (eval-str "(class-of \"test\")") (var-value "String"))
(define-test/eq (eval-str "(class-of [])") (var-value "List"))
(define-test/eq (eval-str "(class-of [1])") (var-value "List"))

;;; cond
(define-test/eq (eval-str "(cond true 1)") (num 1))
(define-test/eq (eval-str "(cond false 1)") fnnull)
(define-test/eq (eval-str "(cond false 1 true 2 true 3)") (num 2))

;;; function and variable def
(define-test/eq (eval-str "(def x 2)") (num 2))
(define-test/eq (eval-str "x") (num 2))
(define-test (eval-str "(def (f x) (* x x))") #'fnfun?)
(define-test/eq (eval-str "(f x)") (num 4))
(define-test (eval-str "(def (g x) (if (= x 3) \"x is 3!\" null))"))
(define-test/eq (eval-str "(g 3)") "x is 3!")
(define-test/eq (eval-str "(g 4)") fnnull)

;;; defclass & object creation
(define-test (eval-str "(defclass (Vec2 x y))") #'fnclass?)
(define-error-test (eval-str "(Vec2 1 -1)") :no-error t)
;; class-of on Vec2
(define-test/eq (eval-str "(class-of (Vec2 0 0))") (eval-str "Vec2"))

;;; defmacro and macroexpansion
(define-error-test (eval-str "(defmacro mac ''macro-time)")
    :message "defmacro: Malformed macro prototype")
(define-test (eval-str "(defmacro (mac) ''macro-time)"))
(define-test/eq (eval-str "(mac)") (fnintern "macro-time"))
;; expansion with gensym
(define-test (eval-str "(defmacro (call-twice fun arg)
                          (let ($arg (gensym)
                                $fun (gensym))
                            `(let (,$arg ,arg
                                   ,$fun ,fun)
                               (do (,$fun ,$arg) (,$fun ,$arg)))))"))
(define-test/eq
    (eval-str "(let (x 1)
                 (call-twice show (set x (+ x 1))))")
    "2.0")
;; also ensure that alternative behavior works as expected
(define-test/eq
    (eval-str "(let (x 1)
                 (show (set x (+ x 1)))
                 (show (set x (+ x 1))))")
    "3.0")
;; expansion with variable capture
(define-test (eval-str "(defmacro (bind-it v & args)
                          `(let (it ,v)
                             ,@args))"))
(define-test/eq (eval-str "(bind-it (+ 2 4) (/ it 3))") (num 2))

;;; defmethod, method definition, and method calls
(define-test (eval-str "(defmethod ((m obj) obj))") #'fnmethod?)
(define-test (eval-str "(def ((m Num) x) (String \"(num \" x \")\"))"))
(define-test/eq (eval-str "(m 2)") "(num 2.0)")
(define-error-test (eval-str "(m \"str\")")
    :message "Runtime error: m: Method not implemented on types (String).")

;;; defvar and instantiation
(define-test (eval-str "(defvar v (Vec2 1 -1))"))
(define-test/eq (eval-str "(= v (Vec2 1 -1))") true)

;;; do
;; return last arg
(define-test/eq (eval-str "(do false true)") true)
(define-test/eq (eval-str "(do false)") false)
;; evaluate expressions in order
(define-test/eq (eval-str "(let (x 0)
                             (do (set x (+ x 4))
                                 (set x (* x 2))))")
    (num 8))
(define-test/eq (eval-str "(let (x 0)
                             (do (set x (* x 2))
                                 (set x (+ x 4))))")
    (num 4))

;;; dollar-fn
;; no arguments
(define-test (eval-str "$(+ 2 2)") #'fnfun?)
(define-test/eq (eval-str "($(+ 2 2))") (num 4))
(define-error-test (eval-str "($(+ 2 2) 2)")
    :message "Runtime error: #<Function>: Too many arguments")
;; fixed # of arguments
(define-test (eval-str "$(+ $ 2)") #'fnfun?)
(define-test/eq (eval-str "($(+ $ 2) 2)") (num 4))
(define-error-test (eval-str "($(+ $ 2))")
    :message "Runtime error: #<Function>: Too few arguments")
(define-error-test (eval-str "($(+ $ 2) 2 3)")
    :message "Runtime error: #<Function>: Too many arguments")
;; variadic args
(define-test (eval-str "$(apply List $&)") #'fnfun?)
(define-test/eq (eval-str "($(apply List $&))") empty)
(define-error-test (eval-str "($(apply List $ $&))")
    :message "Runtime error: #<Function>: Too few arguments")
;; dollar-bracket
(define-test (eval-str "$[$1 $0]") #'fnfun?)
(define-test/eq (eval-str "($[$1 $0] 1 2)") (fnlist (num 2) (num 1)))
;; dollar-backtick
(define-test (eval-str "$`($)") #'fnfun?)
;; check that quoted dollars aren't detected as arguments
(define-test/eq (eval-str "($`($))") (fnlist (fnintern "$")))
(define-error-test (eval-str "($`(,$))")
    :message "Runtime error: #<Function>: Too few arguments")
(define-test/eq (eval-str "($`(,$) 1)") (fnlist (num 1)))
;; dollar-brace not yet implemeted

;;; fn
(define-test (eval-str "(fn () 2)") #'fnfun?)
(define-test/eq (eval-str "((fn () 2))") (num 2))
(define-test/eq (eval-str "((fn (x (y 2)) [x y]) 1)") (fnlist (num 1) (num 2)))
(define-test/eq (eval-str "((fn (x (y 2)) [x y]) 1 1)") (fnlist (num 1) (num 1)))
(define-test/eq (eval-str "((fn (x ('y 2)) [x y]) 1)") (fnlist (num 1) (num 2)))
(define-test/eq (eval-str "((fn (x ('y 2)) [x y]) 1 'y 3)") (fnlist (num 1) (num 3)))
(define-test/eq (eval-str "((fn (& args) [(head args) (head (tail (tail args)))]) 1 2 -1 -2)")
    (fnlist (num 1) (num -1)))

;;; get
(define-test/eq (eval-str "(get (Vec2 1 -1) 'x)") (num 1))
(define-test/eq (eval-str "v.y") (num -1))
(define-error-test (eval-str "v.z")
    :message "Runtime error: get: Object has no field named z")

;;; if
(define-test/eq (eval-str "(if true 'yes 'no)") (fnintern "yes"))
(define-test/eq (eval-str "(if false 'yes 'no)") (fnintern "no"))

;;; let
(define-test/eq (eval-str "(let (x 2) x)") (num 2))
(define-test/eq (eval-str "(let (x 2 y '%) (String x y))") (fnstring "2.0%"))
;; recursion
(define-test/eq
    (eval-str "(let (f (fn (acc src)
          (if (empty? src) acc (f (cons (head src) acc) (tail src)))))
  (f [] [1 2 3]))")
    (fnlist (num 3) (num 2) (num 1)))
;; mutual recursion
(define-test/eq
    (eval-str "(let (f (fn (acc src)
          (if (empty? src) acc (g (cons (head src) acc) (tail src))))
      g (fn (acc src)
          (if (empty? src) acc (f (cons (head src) acc) (tail src)))))
  (g [] [0 1 2 3]))")
    (fnlist (num 3) (num 2) (num 1) (num 0)))

;;; quasiquote
(define-test/eq (eval-str "`2") (num 2))
(define-test/eq (eval-str "`,2") (num 2))
(define-test/eq (eval-str "`(+ 3 4)") (fnlist (fnintern "+") (num 3) (num 4)))
(define-test/eq (eval-str "`,(+ 3 4)") (num 7))
(define-test/eq (eval-str "`(,@[1 2])") (fnlist (num 1) (num 2)))
(define-test/eq (eval-str "``(,@[1 2])")
    (fnlist (fnintern "quasiquote")
            (fnlist
             (fnlist (fnintern "unquote-splice")
                     (fnlist (fnintern "List") (num 1) (num 2))))))
(define-test/eq (eval-str "``(,(,@[1 2]))")
    (fnlist (fnintern "quasiquote")
            (fnlist
             (fnlist (fnintern "unquote")
                     (fnlist (num 1) (num 2))))))

;;; quote
(define-test/eq (eval-str "'symbol") (fnintern "symbol"))
(define-test/eq (eval-str "'true") (fnintern "true"))
(define-test/eq (eval-str "'[]") (fnlist (fnintern "List")))
(define-test/eq (eval-str "'2") (num 2))
(define-test/eq (eval-str "'\"2\"") "2")
(define-test/eq (eval-str "'(a b)") (fnlist (fnintern "a") (fnintern "b")))

;;; set
;; uses (defvar v) from earlier test
(define-error-test (eval-str "(set v (Vec2 0 0))") :no-error t)
(define-test/eq (eval-str "v.x") (num 0))
(define-test/eq (eval-str "v.y") (num 0))
(define-test/eq (eval-str "(set v.x 4)") (num 4))
(define-test/eq (eval-str "v.x") (num 4))

;;; unquote & unquote-splice
(define-error-test (eval-str ",2")
    :message "unquote: unquote outside of quasiquote")
(define-error-test (eval-str ",@2")
    :message "unquote-splice: unquote-splice outside of quasiquote")

;; fresh runtime for running
(let* ((*runtime* (init-runtime))
       (*current-env* (init-env)))
  (run-tests))
