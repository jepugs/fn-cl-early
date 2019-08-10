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
  (:use :cl :fn.util :fn.scanner :fn.ast :fn.parser-gen)
  (:import-from :fn.scanner :line)
  (:export :parse :parse-string :def-fn-parser :make-fn-parser-state))

(in-package :fn.parser)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Grammar and Callback Definitions

(eval-when
    (:compile-toplevel :load-toplevel) 
  (defparameter fn-grammar
    '((program -> (* expr) @eof)
      (expr -> string / number / group / unary / dollar / dot / sym)

      (string -> @string)
      (number -> @number)

      (group -> paren / bracket / brace)
      (paren -> @left-paren (* expr) @right-paren)
      (bracket -> @left-bracket (* expr) @right-bracket)
      (brace -> @left-brace (* expr) @right-brace)

      (unary -> quot / quasiquot / unquot / unquot-splice)
      (quot -> @quote expr)
      (quasiquot -> @backtick expr)
      (unquot -> @comma expr)
      (unquot-splice -> @comma-splice expr)

      (dollar -> @dollar-paren (* expr) @right-paren
       / @dollar-bracket (* expr) @right-bracket
       / @dollar-brace (* expr) @right-brace
       / @dollar-backtick expr)

      (dot -> dot-part sym)
      (dot-part -> sym @dot / dot @dot)
      (sym -> @symbol)

      ;; error productions
      ;; mismatched delimiters
      (mismatch -> (@left-paren / @left-bracket / @dollar-paren / @dollar-bracket)
       (* expr) @right-brace
       / (@left-bracket / @left-brace / @dollar-bracket / @dollar-brace)
       (* expr) @right-paren
       / (@left-paren / @left-brace / @dollar-paren / @dollar-brace) (* expr)
       @right-bracket)
      ;; unclosed delimiters
      (unclosed -> (@left-paren / @left-bracket / @left-brace / @dollar-paren / @dollar-bracket
                    / @dollar-brace)
       (* expr) @eof)
      ;; illegal dot operands
      (illegal-dot -> (string / number / group / unary / dollar ) @dot
       / (@dot / dot-part) (string / number / group / unary / dollar))
      ;; illegal dot syntax
      (dot-syntax -> (@left-paren / @left-bracket / @left-brace / @dollar-paren / @dollar-bracket
                      / @dollar-brace)
       @dot
       / (@right-paren / @right-bracket / @right-brace / @dot / @eof) @dot)
      ;; missing unary operand
      (missing-unary -> (@quote / @quasiquot / @unquot / @unquot-splice)
       (@right-paren / @right-brace / @right-bracket / @eof)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter fn-grammar-callbacks
    '((program $(butlast $))
      (expr #'car)

      (string $(ast-string (token-origin (car $)) (token-datum (car $))))
      (number $(ast-number (token-origin (car $)) (token-datum (car $))))

      (group #'car)
      (paren $(ast-paren (token-origin (car (last $))) (butlast (cdr $))))
      (bracket $(ast-bracket (token-origin (car (last $))) (butlast (cdr $))))
      (brace $(ast-brace (token-origin (car (last $))) (butlast (cdr $))))

      (unary #'car)
      (quot $(ast-quot (slot-value (cadr $) 'origin) (cadr $)))
      (quasiquot $(ast-quasiquot (slot-value (cadr $) 'origin) (cadr $)))
      (unquot $(ast-unquot (slot-value (cadr $) 'origin) (cadr $)))
      (unquot-splice $(ast-unquot-splice (slot-value (cadr $) 'origin) (cadr $)))

      (dollar $(let ((l (token-origin (car (last $)))))
                 (ast-dollar
                  l
                  (case (token-kind (car $))
                    ((dollar-paren) (ast-paren l (butlast (cdr $))))
                    ((dollar-bracket) (ast-bracket l (butlast (cdr $))))
                    ((dollar-brace) (ast-brace l (butlast (cdr $))))
                    ((dollar-backtick) (ast-quasiquot l (cadr $)))))))

      (dot $(ast-dot (slot-value (cadr $) 'origin) (car $) (cadr $)))
      (dot-part #'car)
      (sym $(ast-sym (token-origin (car $)) (token-text (car $))))

      (mismatch $(let ((op (car $))
                       (cl (car (last $))))
                   (fn-error (token-origin cl)
                             "mismatched delimiter ~s (opening delimiter ~s at line ~a, col ~a)"
                             (token-text cl)
                             (token-text op)
                             (origin-line (token-origin op))
                             (origin-column (token-origin (car $))))))
      (unclosed $(fn-error (slot-value (car (last $)) 'origin)
                           "unclosed delimiter (opening delimiter ~s at line ~a, col ~a)"
                           (token-text (car $))
                           (origin-line (token-origin (car $)))
                           (origin-column (token-origin (car $)))))
      (illegal-dot $(fn-error (slot-value (cadr $) 'origin)
                     "dot operator can only be used with symbols"))
      (dot-syntax $(fn-error (slot-value (cadr $) 'origin)
                             "illegal dot syntax"))
      (missing-unary $(fn-error (slot-value (cadr $) 'origin)
                                "missing operand for ~a"
                                (token-text (car $)))))))

(defmacro make-fn-parser-state (str)
  `(make-parser (coerce (scan-from-string ,str) 'list)
       ,fn-grammar
     ,@fn-grammar-callbacks))

(defmacro def-fn-parser (name)
  `(defparser ,name ,fn-grammar ,@fn-grammar-callbacks))

(def-fn-parser parse)

(defun parse-string (str)
  (parse (scan-from-string str)))
