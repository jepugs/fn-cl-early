(provide 'fn-mode)

(require 'lisp-mode)

(defvar fn-mode-hook nil)

(defvar fn-mode-map
  (let ((map (copy-keymap lisp-mode-map)))
    (make-keymap)))

(add-to-list 'auto-mode-alist '("\\.fn\\'" . fn-mode))

(defvar fn-definers
  '("def" "def*" "defimpl" "defmacro" "defn" "defproto" "deftype" "defvar")
  "Definer forms have syntax highlighting on the operator and the
 first argument. Their bodies are indented two spaces past the
 previous one.")
(defvar fn-special-operators
  '("@" "and" "case" "cond" "do" "if" "fn" "let" "new" "or" "progn" "set!")
  "Special operators that have their names highlighted")
(defvar fn-constants
  '("$" "$0" "$1" "$2" "$3" "$4" "$5" "&" "True" "False" "Null" "_"))

;; indentation specifiers for certain operators
(defvar fn-operator-indent-alist
  '(("case" 1 4 cycle 2 4)
    ("cond" 1 cycle 2 4)
    ("def" 1 cycle 2 4)
    ("def*" 1 cycle 2 4)
    ("deftype" 1 4 4 cycle 2 4)
    ("defimpl" 1 4 4 cycle 2 4)
    ("defmacro" 1 4 . 2)
    ("defn" 1 4 4 . 2)
    ("defproto" 1 4 4 cycle 2 4)
    ("defvar" 1 cycle 2 4)
    ("do" 1 . 2)
    ("if" 1 4 2 . 2)
    ("fn" 1 4 . 2)
    ("let" 1 4 . 2)
    ("progn" 1 . 2)
    ("set!" 1 cycle 2 4)))

(defvar fn-fallback-indenter 2)

(defvar fn-font-lock-keywords
  (list
   ;; definers
   (list (concat
          "("
          (regexp-opt fn-definers t)
          "\\_>\\(?:\\(?:\\s-\\|\n\\)+\\)?\\(\\_<\\(?:\\s_\\|\\sw\\)+\\_>\\)?")
         '(1 font-lock-keyword-face)
         '(2 font-lock-variable-name-face))
   ;; other special operators
   (list (concat "("
                 (regexp-opt fn-special-operators t)
                 "\\_>")
         1 'font-lock-keyword-face)
   (cons (concat "\\_<"
                 (regexp-opt fn-constants t)
                 "\\_>")
         'font-lock-constant-face)
   (list (concat "\\(:\\(?:\\sw\\|\\s_\\)+\\)")
         1 'font-lock-builtin-face)))

(defvar fn-mode-syntax-table
  (let ((stab (make-syntax-table)))
    ;; numbers
    (modify-syntax-entry ?0 "w" stab)
    (modify-syntax-entry ?1 "w" stab)
    (modify-syntax-entry ?2 "w" stab)
    (modify-syntax-entry ?3 "w" stab)
    (modify-syntax-entry ?4 "w" stab)
    (modify-syntax-entry ?5 "w" stab)
    (modify-syntax-entry ?6 "w" stab)
    (modify-syntax-entry ?7 "w" stab)
    (modify-syntax-entry ?8 "w" stab)
    (modify-syntax-entry ?9 "w" stab)
    ;; symbol characters
    (modify-syntax-entry ?- "_" stab)
    (modify-syntax-entry ?_ "_" stab)
    (modify-syntax-entry ?@ "w" stab)
    (modify-syntax-entry ?: "_" stab)
    (modify-syntax-entry ?! "w" stab)
    (modify-syntax-entry ?* "w" stab)
    (modify-syntax-entry ?. "_" stab)
    ;; delimiters
    (modify-syntax-entry ?\( "()" stab)
    (modify-syntax-entry ?\) ")(" stab)
    (modify-syntax-entry ?\[ "(]" stab)
    (modify-syntax-entry ?\] ")[" stab)
    (modify-syntax-entry ?\{ "(}" stab)
    (modify-syntax-entry ?\} "){" stab)
    ;; comments
    (modify-syntax-entry ?\; "<" stab)
    (modify-syntax-entry ?\n ">" stab)
    ;; other characters
    (modify-syntax-entry ?\\ "\\" stab)
    (modify-syntax-entry ?\" "\"" stab)
    (modify-syntax-entry ?\' "'" stab)
    (modify-syntax-entry ?\` "'" stab)
    (modify-syntax-entry ?\, "'" stab)
    (modify-syntax-entry ?\# "'" stab)
    (modify-syntax-entry ?\$ "'" stab)
    stab))

;; FIXME: rename to get 
(defun fn--read-next-sexp ()
  "Get the next sexp as a string. Behavior is determined by the
 forward-sexp and backward-sexp functions."
  (let ((beg (point))
        (end (point)))
    (save-excursion
      (forward-sexp)
      (setq end (point))
      (backward-sexp)
      (setq beg (point)))
    (if (= beg end)
        ""
      (buffer-substring-no-properties beg end))))

(defun fn--next-sexp-col ()
  "Get the column where the next sexp starts."
  (save-excursion
    (forward-sexp)
    (backward-sexp)
    (current-column)))

;; FIXME: rename to fit conventions
(defun fn--count-sexps-before (pos)
  "Count the number of sexps between point and pos."
  (save-excursion
    (do ((n 0 (+ n 1)))
        ((> (point) pos) (- n 1))
      (condition-case nil
          (forward-sexp)
        ;; assume this error means we're at the end of the list
        (error (return n))))))

(defun fn--in-a-string (parser-state)
  (nth 3 parser-state))

(defun fn--at-toplevel (parser-state)
  (zerop (car parser-state)))

;; FIXME: deprecated
(defun fn-surrounding-list ()
  "Find out information about the list we're currently in.
 Returns a list (DELIM-CHAR DELIM-COL OPERATOR NUMBER CADR-POS)
 or nil if we're on the top level. DELIM-CHAR and DELIM-COL are
 the delimiter that opened this list and its column number.
 OPERATOR is a string, the first sexp in the list. NUMBER is the
 index (from 0) in the list of the next sexp after the mark (even
 if it's not there yet). Finally, CADR-POS is column number of
 the second element of the list, used for standard lisp-style indentation."
  ;; first, check if we're inside quotes
  (if (nth 3 (syntax-ppss))
      '(?\" 0 "" 0)
    (let ((d-ch nil)
          (d-col 0)
          (op "")
          (n 0)
          (start (point))
          (c nil))
      (save-excursion
        (condition-case nil
            (progn
              (backward-up-list)
              (setq d-ch (char-after))
              (setq d-col (current-column))
              (forward-char)
              (setq op (fn--read-next-sexp))
              (setq n (fn--count-sexps-before start))
              ;; see if there is a CADR to set CADR-POS
              (if (> n 0)
                  (progn
                    ;; go twice forward and once back to arrive at the beginning of the sexp
                    (forward-sexp)
                    (forward-sexp)
                    (backward-sexp)
                    (setq c (current-column)))))
          ;; assume this is a scan error, meaning we're at the top level
          (error
           (setq d-ch nil)
           (setq d-col 0)
           (setq op "")
           (setq n 0)
           (setq c nil))))
      (list d-ch d-col op n c))))

;;; FIXME: change contexts to be records of type fn-context

;;; FIXME: this could be accomplished much more efficiently using the parser state from syntax-ppss.
;;; function should be rewritten from scratch.
(defun fn--current-context ()
  "A context describes the (code) list containing the point. It
 includes information about the operator of the current list, the
 number of elements in the list before the point, the delimiter
 used to open the list, and the columns numbers where the
 delimiter, first, and second items of the list reside."
  ;; first, check if we're inside quotes
  (if (fn--in-a-string (syntax-ppss))
      '(?\" 0 "" 0)
    (let ((delim nil)
          (delim-col 0)
          (op nil)
          (op-col nil)
          (list-pos 0)
          (cadr-col nil)
          (start (point)))
      (save-excursion
        (condition-case nil
            (progn
              (backward-up-list)
              (setq delim (char-after))
              (setq delim-col (current-column))
              (forward-char)
              (setq list-pos (fn--count-sexps-before start))
              ;; if the first element of the list is after the point, then we set the operator
              ;; column to one space after the delimiter.
              (cond ((= list-pos 0)
                     (setq op-col (+ delim-col 1)))
                    (t
                     (setq op (fn--read-next-sexp))
                     (setq op-col (fn--next-sexp-col))))
              ;; see if there is a CADR to set CADR-POS
              (when (> list-pos 1)
                (forward-sexp)
                (setq cadr-col (fn--next-sexp-col))))
          ;; assume this is a scan error, meaning we're at the top level
          (error)))
      (list delim delim-col op op-col list-pos cadr-col))))


;; I should probably use defclass or cl-defstruct or something but it's late and I hate learning
(defun context-delim (cxt) (car cxt))
(defun context-delim-col (cxt) (cadr cxt))
(defun context-op (cxt) (caddr cxt))
(defun context-op-col (cxt) (cadddr cxt))
(defun context-list-pos (cxt) (nth 4 cxt))
(defun context-cadr-col (cxt) (nth 5 cxt))


(defun fn-find-offset (indenter index)
  "Use an indenter to decide the offset of an element in an fn
 code list. Index is the index of the element in question.
 Returns an offset."
  (let* ((is-cycle (and (listp indenter)
                        (eq (car indenter) 'cycle)))
         (is-atom (or (atom indenter) is-cycle)))
    (cond ((functionp indenter) (funcall indenter index))
          (is-atom
           (if is-cycle
               (nth (mod index (- (length indenter) 1))
                    (cdr indenter))
             indenter))
          ((zerop index)
           (car indenter))
          (t (fn-find-offset (cdr indenter) (- index 1))))))

(defun fn-lookup-indenter (delim op)
  (cond
   ((null delim) 0)
   ((eql delim ?\()
    (let ((c (assoc op fn-operator-indent-alist 'equal)))
      (if c
          (cdr c)
        fn-fallback-indenter)))
   ((eql delim ?\[) 1)
   ((eql delim ?\{) '(cycle 1 3))
   ((eql delim ?\") nil)
   (t fn-fallback-indenter)))

(defun fn--compute-indent-amount (cxt &optional offset)
  "Using the provided context, compute the standard lisp-style
  indentation level. In most cases, this will line the point up
  with the second element of the list, in standard lisp style.
  The exception is when a custom indenter is defined for the
  given operator/delimiter. Offset should be a number or nil. In
  the former case, the line is indented that many columns beyond
  the first character of the car of the containing list.
  Otherwise, it is aligned with the cadr of the list (in the
  usual lisp style). Eventually this will support custom
  indentation for certain forms."
  ;; TODO: add check for custom indenters

  (if offset
      (+ offset (context-op-col cxt))
    ;; null offset means use standard indentation rules.
    (or (context-cadr-col cxt)
        (context-op-col cxt))))

;; nil value indicates previous indent was a standard lisp indent/custom indenter
(defvar fn--prev-indent-offset nil)
(defun fn--next-indent-offset ()
  (case fn--prev-indent-offset
    ((0) nil)
    ((1) 0)
    (t 1)))

(defun fn--indent-line (&optional again)
  "Internal indentation function. kind should be 'nil or a number"
  (if again
      (setq fn--prev-indent-offset (fn--next-indent-offset))
    (setq fn--prev-indent-offset nil))
  (let ((start (point)))
    (beginning-of-line)
    (let ((cxt (fn--current-context))
          (offset (when again fn--prev-indent-offset)))
      (goto-char start)
      (indent-line-to (fn--compute-indent-amount cxt offset)))))


(defun fn-indent-line ()
  "Indent current line as fn code"
  (interactive "*")
  (fn--indent-line (eq last-command this-command)))

(defun fn-mode ()
  "Major mode for editing fn programming language source code."
  (interactive)
  (kill-all-local-variables)
  (set-syntax-table fn-mode-syntax-table)
  (use-local-map fn-mode-map)
  (set (make-local-variable 'font-lock-defaults)
       '(fn-font-lock-keywords nil nil))
  (set (make-local-variable 'fn--prev-indent)
       'standard)
  ;;(set (make-local-variable 'indent-line-function) 'fn-indent-line)
  (setq-local indent-line-function 'fn-indent-line)
  (setq major-mode 'fn-mode)
  (setq mode-name "fn")
  (setq-local comment-start "; ")
  (setq-local commend-end "")
  (run-hooks 'fn-mode-hook))
