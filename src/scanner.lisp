;;;; scanner.lisp -- tokenizes fn source code

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

(defpackage :fn.scanner
  (:documentation "tokenizes fn source code")
  (:use :cl :fn.util)
  (:export
   ;; Token kinds
   :token-kinds :left-paren :right-paren :left-bracket :right-bracket :left-brace :right-brace
   :quote :backtick :comma :comma-splice :dot :number :string :symbol :eof :dollar-paren
   :dollar-bracket :dollar-brace :dollar-backtick
   ;; Token structure
   :token :token-kind :token-origin :token-datum :token-text
   ;; Main scanner functions
   :scan :scan-from-string
   ))

(in-package :fn.scanner)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Tokens

(defparameter token-kinds
  '(left-paren right-paren left-bracket right-bracket left-brace right-brace
    quote backtick comma comma-splice dot
    dollar-paren dollar-bracket dollar-brace dollar-backtick
    number string symbol
    eof))

(defstruct token
  "Tokens output by the scanner"
  kind
  datum
  origin
  text)

(defun fmt-token (tok)
  "Convert a token to human-readable form and return it as a string. Used for debugging."
  (with-slots (kind datum) tok
    (if datum
        (format nil "~s(~s)" kind datum)
        (princ-to-string kind))))

(defun print-tokens (tokens)
  "Pretty-print a series of tokens. Used for debugging."
  (format t "~{~a~^~%~}" (map 'list #'fmt-token tokens)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Scanner state

(defstruct scanner-state
  stream
  filename
  (line 1)
  (column 0)
  (cur-tok (make-array 16 :fill-pointer 0 :adjustable t))
  (tokens (make-array 16 :fill-pointer 0 :adjustable t)))

(defun scanner-origin (ss)
  (with-slots (filename line column) ss
    (make-origin :filename filename
                 :line line
                 :column column)))


(defun consume (ss)
  "Advance the reader head one character, adding that character to the current token. Returns the
 character or :EOF if the end of the file has been reached."
  (let ((c (read-char (scanner-state-stream ss) nil :eof)))
    ;; don't add to the token if this is EOF
    (unless (eq c :eof)
      (if (eql c #\newline)
          (progn (incf (scanner-state-line ss))
                 (setf (scanner-state-column ss) 0))
          (incf (scanner-state-column ss)))
      (vector-push-extend c (scanner-state-cur-tok ss)))
    c))

(defun skip-char (ss)
  "Advance the reader head without adding the character to the current token."
  (let ((c (read-char (scanner-state-stream ss) nil :eof)))
    (if (eql c #\newline)
        (progn (incf (scanner-state-line ss))
               (setf (scanner-state-column ss) 0))
        (incf (scanner-state-column ss)))
    c))

(defun peek (ss)
  "Look at the next character without moving the reader head. Returns :EOF if the end of the file
 has been reached."
  (peek-char nil (scanner-state-stream ss) nil :eof))

(defun current-token (ss)
  "Get the current token as a string."
  (coerce (scanner-state-cur-tok ss) 'string))

(defun add-token (ss kind &optional datum)
  "Add the current token to the end of the stream."
  (with-slots (cur-tok tokens) ss
    (vector-push-extend (make-token :kind kind
                                    :datum datum
                                    :origin (scanner-origin ss)
                                    :text (coerce cur-tok 'string))
                        tokens)
    (setf (fill-pointer cur-tok) 0)))

(defun add-char-token (ss kind &optional datum)
  "Consume one character and then add a token. This is a convenience function used to create
 single- and double-character tokens."
  (consume ss)
  (add-token ss kind datum))

(defun at-eof? (ss)
  "Tell if the scanner state has reached the end of the file"
  (eq (peek-char nil (scanner-state-stream ss) nil :eof) :eof))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Scanner logic

(defun symbol-char? (c)
  "Characters that can be constituents in atoms."
  (and (characterp c)
       (or (alphanumericp c)
           (member c (coerce "\\~!@#$%^&*-_+=|:<>/?" 'list) :test #'eql))))

(defun whitespace? (c)
  "Whether C is a whitespace character."
  (or (eq c :eof)
      (member c '(#\newline #\return #\tab #\space) :test #'eql)))

(defun num-first-char? (c)
  "Whether a number can begin with character C."
  (member c (coerce "1234567890-+" 'list) :test #'eql))

(defun digit-char? (c)
  (and (characterp c)
       (digit-char-p c)))

(defun scan-token (ss)
  "Get the next token from the scanner, updating the state in the process."
  (let ((c (peek ss)))
    (case c
      ;; end of file
      ((:eof) nil)

      ;; single-character tokens
      ((#\() (add-char-token ss 'left-paren))
      ((#\)) (add-char-token ss 'right-paren))
      ((#\[) (add-char-token ss 'left-bracket))
      ((#\]) (add-char-token ss 'right-bracket))
      ((#\{) (add-char-token ss 'left-brace))
      ((#\}) (add-char-token ss 'right-brace))
      ((#\') (add-char-token ss 'quote))
      ((#\`) (add-char-token ss 'backtick))
      ((#\.) (add-char-token ss 'dot))

      ;; single- or double-character tokens
      ((#\,) (consume ss)
       (if (eql (peek ss) #\@)
           (add-char-token ss 'comma-splice)
           (add-token ss 'comma)))
      ((#\$)
       (consume ss)
       (case (peek ss)
         ((#\() (add-char-token ss 'dollar-paren))
         ((#\[) (add-char-token ss 'dollar-bracket))
         ((#\{) (add-char-token ss 'dollar-brace))
         ((#\`) (add-char-token ss 'dollar-backtick))
         ;; default to treating this as a symbol constituent
         (t (scan-sym ss))))

      ;; comments
      ((#\;)
       (do ((c (skip-char ss) (skip-char ss)))
           ((or (eql c #\newline)
                (eql c :eof)))))

      ;; strings
      ((#\") (scan-string-literal ss))

      (t
       (cond ((whitespace? c)
              (skip-char ss)
              ;; call recursively
              (scan-token ss))
             ;; numbers and strings
             (t (scan-num-or-sym ss)))))))

(defun scan-escape-code (ss)
  "Read an escape code"
  (let ((c (consume ss)))
    (case c
      ;; single-character codes
      ((#\a) #\bel)
      ((#\b) #\backspace)
      ((#\e) #\esc)
      ((#\n) #\newline)
      ((#\r) #\return)
      ((#\t) #\tab)
      ((#\v) #\vt)
      ((#\') #\')
      ((#\") #\")
      ((#\\) #\\)
      ((#\?) #\?)

      ((#\0) #\nul)

      ;; for now, we'll forget about the fancy octal and hexadecimal escape codes
      (t (fn-error (scanner-origin ss)
                   "unrecognized string escape code: \"\\~s\""
                   c)))))

(defun scan-string-literal (ss)
  "Scan a string literal."
  ;; skip the leading "
  (consume ss)
  (rlambda (acc escaped?) (nil nil)
    (let ((c (peek ss)))
      (cond
        ;; emit an error if the stream ends before the string is finished
        ((at-eof? ss) (fn-error (scanner-origin ss)
                                "reached EOF while scanning string"))

        ;; when we're escaped, we dispatch to the scan-escape-code helper
        (escaped?
         (recur (cons (scan-escape-code ss) acc) nil))

        ;; start an escape sequence
        ((eql c #\\)
         (consume ss)
         (recur acc t))

        ;; end string
        ((eql c #\")
         (consume ss)
         (add-token ss 'string (coerce (nreverse acc) 'string)))

        ;; in this case we just carry on readying
        (t (recur (cons (consume ss) acc) nil))))))

(defun scan-num-or-sym (ss)
  "Called to get either a number or a symbol from the scanner. This function will throw an error if
 neither a valid symbol nor a valid string can be read."
  (aif (try-scan-num ss)
       (add-token ss 'number it)
       (scan-sym ss)))

(defun try-scan-num (ss)
  "Attempt to read a number token from the scanner stream. If the stream does not begin with a
 well-formed number, returns NIL but does not throw an error. Otherwise, returns the scanned number
 as a double float."
  (let ((c (peek ss)))
    (case c
      ;; check for a sign character
      ((#\+)
       (consume ss)
       (if (digit-char? (peek ss))
           (try-scan-num-helper ss 1)
           nil))
      ((#\-)
       (consume ss)
       (if (digit-char? (peek ss))
           (try-scan-num-helper ss -1)
           nil))

      ;; check for a digit
      ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
       (try-scan-num-helper ss 1))

      ;; otherwise return null
      (t nil))))

(defun try-scan-num-helper (ss sign)
  ;; start by scanning all the characters involved
  (let ((chars
         (rlambda (int-acc frac-acc point?) (nil nil nil)
           (let ((c (peek ss)))
             (cond
               ((eql c #\.)
                (if point?
                    (fn-error (scanner-origin ss)
                              "Ambiguous combination of dots and numbers")
                    (progn (consume ss)
                           (recur int-acc frac-acc t))))

               ((digit-char? c)
                (consume ss)
                (if point?
                    (recur int-acc (cons c frac-acc) point?)
                    (recur (cons c int-acc) frac-acc point?)))

               ;; throw everything out; we're reading a symbol
               ((symbol-char? c)
                (if point?
                    (fn-error (scanner-origin ss)
                              "Ambiguous combination of dots and numbers")
                    nil))

               ;; done reading I guess. Return integer and fractional parts as lists of characters
               ;; in backwards order.
               (t (list int-acc frac-acc)))))))
    (if chars
        ;; successful scan, time to turn it into a string
        ;; separate integer and fractional parts and turn them into numbers
        (let* ((int-digits (mapcar $(coerce (- (char-code $) (char-code #\0))
                                            'double-float)
                                   (car chars)))
               (frac-digits (mapcar $(coerce (- (char-code $) (char-code #\0))
                                             'double-float)
                                    (cadr chars)))
               (int-part (reduce (lambda (l r)
                                   (+ (* l 10) r))
                                 (reverse int-digits)
                                 :initial-value 0))
               (frac-part (* 0.1 (reduce (lambda (l r)
                                           (+ (* l 0.1) r))
                                         frac-digits
                                         :initial-value 0))))
          (* sign (+ int-part frac-part)))
        nil)))

(defun scan-sym (ss)
  "Scan the stream to get a symbol"
  ;; consume all the symbol characters first
  (rlambda (escaped?) (nil)
    (let ((c (peek ss)))
      (cond
        (escaped?
         (consume ss)
         (recur nil))
        ((eql c #\\)
         (consume ss)
         (recur t))
        ((symbol-char? c)
         (consume ss)
         (recur nil)))))
  ;; extract a symbol name from the current token
  (let* ((ct (scanner-state-cur-tok ss))
         (name
          (rlambda (acc escaped? i) (nil nil 0)
            (if (>= i (length ct))
                (if escaped?
                    (fn-error (scanner-origin ss)
                              "symbol name cannot end with \\")
                    (coerce (nreverse acc) 'string))
                (let ((c (aref ct i)))
                  (cond
                    (escaped? (recur (cons c acc) nil (1+ i)))
                    ((eql c #\\) (recur acc t (1+ i)))
                    (t (recur (cons c acc) nil (1+ i)))))))))
    (add-token ss 'symbol name)))

(defun scan (stream &optional filename)
  "Scan the stream until eof is encountered."
  (let ((ss (make-scanner-state :stream stream
                                :filename filename)))
    (loop until (at-eof? ss)
       do (scan-token ss))
    (add-token ss 'eof)
    (scanner-state-tokens ss)))

(defun scan-from-string (string)
  "Scan tokens from a string."
  (scan (make-string-input-stream string)))
