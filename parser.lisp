;;;; parser.lisp -- parser for fn

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

(defpackage :fn.parser
  (:documentation "parser for fn")
  (:use :cl :fn.util :fn.scanner)
  (:export :/))

(in-package :fn.parser)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Parser overview

;;; This is an LR(0) parser. It is implemented using a macro-based DSL that outputs Common Lisp
;;; code. The parser accepts a sequence of tokens as its input.


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

;; everything on the stack is a PSTRING. Tokens are wrapped in PSTRINGs as the first step in
;; parsing.
(defstruct pstring
  "Parser string object used internally. VALUE is the payload of this object. NONTERMINAL refers to
  production used to produce this object. If NONTERMINAL is NIL then it indicates that this is
  actually a character (i.e. a terminal)."
  nonterminal
  value)

(defun terminal? (p)
  "A terminal is a symbol whose name starts with @"
  (and (symbolp p)
       (eql (aref (symbol-name p) 0) #\@)))

(defun or-pattern? (p)
  (and (listp p)
       (member '/ p)))

(defun star-pattern? (p)
  (and (listp p)
       (length= p 2)
       (eq (car p) '*)))

(defun terminal-kind (p)
  (intern (subseq (symbol-name p) 1)))

(defun compile-cat-pattern (rev-p stack)
  "Cat patterns are the concatenated patterns that are created automatically by grouping and
 alternative operators. This function takes the patterns to concatenate in reverse (because the
 stack is also reversed compared to original token order)."
  (cond
    ;; null patterns are legal but generate warnings
    ((null rev-p) 0)
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
  (let ((opts (split-at '/ p)))
    `(or ,@(mapcar $(compile-cat-pattern (reverse $) stack)
                   opts))))


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
               (null (pstring-nonterminal (car ,stack)))
               (eq (token-kind (pstring-value
                                (car ,stack)))
                   ',(terminal-kind p)))
          1
          nil))

    ;; matches another nonterminal
    ((and (symbolp p) (not (null p)))
     `(if (and ,stack
               (eq (pstring-nonterminal (car ,stack)) ',p))
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
            (list ,@(mapcar $(make-pstring :nonterminal nil
                                           :value (fn.scanner::make-token :kind $))
                            token-kinds))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Parser machine

