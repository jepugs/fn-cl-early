;;;; parser-gen.lisp -- parser generator for fn

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

(defpackage :fn.parser-gen
  (:documentation "parser generator for fn")
  (:use :cl :fn.util :fn.scanner :fn.ast)
  (:export :defparser :make-parser :/ :-> :kind :line :program))

(in-package :fn.parser-gen)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Parser overview

;;; This file contains code for a recursive-ascent parser for fn. This is done via a
;;; parser-generator macro that can generate parsers reading LR(1) grammars.


;;;; Rule Syntax:
;;; a rule looks like this

;;; (expr -> @symbol / constant / @left-paren (* expr) @right-paren)

;;; The first symbol is the name of the nonterminal being defined, while the second is just
;;; syntactic sugar. The slash (/) is used to denote alternatives. Parentheses are used for
;;; grouping.

;;; The form (* expr) is the kleene star operation on the nonterminal expr.

;;; Symbols with names that begin with @ correspond to nonterminal symbols. These are matched based
;;; upon the KIND of the Token provided (see scanner.lisp). Other symbols correspond to nonterminals
;;; which should be defined elsewhere in the grammar.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Pattern compilation

;; patterns compile to functions that return either NIL or the number of tokens in the matching
;; string. Kinds of patterns:

;; everything on the stack is a POBJ. Tokens are wrapped in POBJs as the first step in parsing.
(defstruct pobj
  "Parser object used internally. Corresponds to either an input token or the result of a reduction.
  VALUE is the payload of this object. NONTERMINAL refers to rule used to produce this object. If
  NONTERMINAL is NIL then it indicates that this is actually a character (i.e. a terminal)."
  nonterminal value)

(defun terminal? (p)
  "A terminal is a symbol whose name starts with @"
  (and (symbolp p)
       (eql (aref (symbol-name p) 0) #\@)))

(defun or-pattern? (p)
  (and (listp p)
       (member '/ p)
       (every #'cat-pattern? (split-at '/ p))
       t))

(defun cat-pattern? (p)
  (and (listp p)
       (every #'pattern? p)
       t))

(defun star-pattern? (p)
  (and (listp p)
       (length= p 2)
       (eq (car p) '*)
       (pattern? (cadr p))))

(defun pattern? (p)
  "Tell if p is a syntactically valid pattern."
  (or (symbolp p)
      (or-pattern? p)
      (star-pattern? p)
      (cat-pattern? p)))

(defun terminal-kind (p)
  "Gets the kind from a terminal"
  (intern (subseq (symbol-name p) 1)))

(defun compile-cat-pattern (rev-p stack)
  "Cat patterns are the concatenated patterns that are created automatically by grouping and
 alternative operators. This function takes the patterns to concatenate in reverse (because the
 stack is also reversed compared to original token order)."
  (cond
    ;; null patterns are legal but generate warnings
    ((null rev-p) nil)
    ;; length-1 lists get optimized away
    ((= (length rev-p) 1)
     (compile-pattern (car rev-p) stack))
    ;; here, we have to do three things:
    ;; - check if the first pattern has a match and remember how many tokens it was
    ;; - check if the rest of the pattern has a match
    ;; - return NIL if either pattern failed, otherwise add the results
    (t
     (with-gensyms (res stack0)
       `(let ((,res ,(compile-pattern (car rev-p) stack)))
          (if ,res
              (let ((,stack0 (drop ,res ,stack))) 
                (aif ,(compile-cat-pattern (cdr rev-p) stack0)
                     (+ it ,res)
                     nil))
              nil))))))

(defun compile-or-pattern (p stack)
  (let ((opts (split-at '/ p))
        (x (gensym)))
    ;; we actually need to check all patterns to ensure that we pick the maximal match
    `(let ((,x (remove-if #'null
                          (list ,@(mapcar $(compile-cat-pattern (reverse $) stack)
                                          opts)))))
       (if ,x (apply #'max ,x)))))


(defun compile-star-pattern (p stack)
  (with-gensyms (s i acc)
    `(do* ((,s ,stack (if ,i (drop ,i ,s) ,s))
           (,i ,(compile-pattern (cadr p) s)
               ,(compile-pattern (cadr p) s))
           (,acc (if ,i ,i 0) (if ,i (+ ,acc ,i) ,acc)))
          ((null ,i) ,acc))))

(defun compile-pattern (p stack)
  "Given a pattern and the name of a stack, returns code that checks whether the pattern matches the
 stack. The code will evaluate to NIL on a non-match and to a number indicating the number of
 consumed tokens on a match."
  (cond
    ;; matches a terminal (token) by its KIND
    ((terminal? p)
     `(if (and ,stack
               (null (pobj-nonterminal (car ,stack)))
               (eq (token-kind (pobj-value
                                (car ,stack)))
                   ',(terminal-kind p)))
          1
          nil))

    ;; matches another nonterminal
    ((and (symbolp p) (not (null p)))
     `(if (and ,stack
               (eq (pobj-nonterminal (car ,stack)) ',p))
          1
          nil))

   

    ;; alternative
    ((or-pattern? p) (compile-or-pattern p stack))

    ((star-pattern? p) (compile-star-pattern p stack))

    ;; concatenate multiple patterns
    ((listp p) (compile-cat-pattern (reverse p) stack))

    (t (error "Unrecognized pattern in grammar rule."))))

(defmacro pattern-fun (pattern)
  "Create a function that checks whether a stack matches the given pattern."
  (with-gensyms (stack)
    `(lambda (,stack)
       ,(compile-pattern pattern stack))))


;;; convenience macro for testing the parser at a REPL
(defmacro test-pattern (pattern token-kinds)
  `(funcall (pattern-fun ,pattern)
            (list ,@(mapcar $(make-pobj :nonterminal nil
                                        :value (fn.scanner::make-token :kind $))
                            token-kinds))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Parser machine

(defstruct parser-state
  parser-table
  stack
  input)


(defun check-rule-syntax (rule)
  "Emit an error if RULE is not a valid production in our DSL."
  (unless (and (listp rule)
               (symbolp (car rule))
               (eq (cadr rule) '->)
               (pattern? (cddr rule)))
    (error "Invalid rule syntax.")))

;;; parser table is actually just an ALIST from patterns to functions
(defun compile-parser-table (grammar callbacks)
  "Generate code for the parser table, which is an ALIST of patterns and callbacks."
  `(list
    ,@(mapcar (lambda (rule)
                (check-rule-syntax rule)
                (aif (assoc (car rule) callbacks)
                     `(cons (pattern-fun ,(cddr rule))
                            (wrap-parser-callback ,(cadr it) ',(car rule)))
                     `(cons (pattern-fun ,(cddr rule))
                            (wrap-parser-callback #'values ',(car rule)))))
              grammar)))

(defun wrap-parser-callback (fun nonterm)
  "Wraps a function so that it converts its arguments from POBJs and converts its result to a POBJ
 with the specified nonterminal."
  (lambda (lst)
    (make-pobj :nonterminal nonterm
               :value (funcall fun
                               (mapcar #'pobj-value lst)))))

(defun find-reduction (ps)
  "Takes a PARSER-STATE. Tells if any patterns in the parser table have a match. Returns a pair
containing the size of the largest match and the callback for the corresponding nonterminal"
  (with-slots (parser-table stack) ps
    (rlambda (max max-fun lst) (-1 nil parser-table)
      (if (null lst)
          (if max-fun (cons max max-fun) nil)
          (let ((x (funcall (caar lst) stack)))
            (if (and x (> x max))
                (recur x (cdar lst) (cdr lst))
                (recur max max-fun (cdr lst))))))))



(defun shift (ps)
  "Shift the parser state one terminal"
  (with-slots (parser-table stack input) ps
    (make-parser-state :parser-table parser-table
                       :stack (cons (car input) stack)
                       :input (cdr input))))

(defun apply-reduction (ps n fun)
  "Reduce the top N items using the callback FUN."
  (with-slots (parser-table stack input) ps
    (let ((obj (funcall fun (reverse (take n stack)))))
     (make-parser-state :parser-table parser-table
                        :stack (cons obj (drop n stack))
                        :input input))))

(defun reduce-all (ps)
  "Perform as many reductions as possible without shifting."
  (do* ((ps0 ps (apply-reduction ps0 (car r) (cdr r)))
        (r (find-reduction ps0) (find-reduction ps0)))
       ((or (null r)
            (= (car r) 0))
        ps0)))

(defun shiftreduce-la (ps0)
  "Perform a shiftreduce with 1-terminal lookahead. This actually tries reducing before shifting and
 may even shift multiple times to ensure that no work is repeated."
  (let* ((r0 (find-reduction ps0))
         (ps1 (shift ps0))
         (r1 (if (null (parser-state-input ps0)) nil (find-reduction ps1))))
    (cond ((null r0)
           (if r1
               (apply-reduction ps1 (car r1) (cdr r1))
               (shift ps1)))
          ((null r1)
           (apply-reduction ps0 (car r0) (cdr r0)))
          ((> (car r1) (car r0))
           (apply-reduction ps1 (car r1) (cdr r1)))
          (t (apply-reduction ps0 (car r0) (cdr r0))))))

(defun validate-halting-stack (stack)
  "Determine if a parser halted with a syntactically correct program. Takes the stack of the halted
 parser as an argument. A valid stack has one PROGRAM nonterminal. If the stack is invalid, it is
 scanned for the first incomplete expression and an error is emitted there."
  (unless (and (length= stack 1)
               (eq (slot-value (car stack) 'nonterminal) 'program))
    (let ((err-elt (find-if-not $(eq (slot-value $ 'nonterminal) 'expr)
                                stack
                                :from-end t)))
      (fn-error (slot-value (slot-value err-elt 'value)
                            'origin)
                "Parsing failed due to incomplete expression")))
  stack)

(defmacro make-parser (input grammar &body callbacks)
  `(make-parser-state :parser-table ,(compile-parser-table grammar callbacks)
                      :stack nil
                      :input (mapcar $(make-pobj :nonterminal nil
                                                 :value $)
                                     ,input)))

(defmacro defparser (name grammar &body callbacks)
  (with-gensyms (tokens ps)
    `(defun ,name (,tokens)
       (let ((,ps (make-parser-state :parser-table ,(compile-parser-table grammar callbacks)
                                     :stack nil
                                     :input (map 'list
                                                 $(make-pobj :nonterminal nil
                                                             :value $)
                                                 ,tokens))))
         (-> (run-parser ,ps)
           (parser-state-stack)
           (validate-halting-stack)
           (car)
           (pobj-value))))))

(defun run-parser (initial-state)
  "Fully reduce the parser"
  (do ((ps (shiftreduce-la initial-state) (shiftreduce-la ps)))
      ((null (parser-state-input ps))
       (reduce-all ps))))

