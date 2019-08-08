(provide 'fn-mode)

(require 'lisp-mode)

(defvar fn-mode-hook nil)

(defvar fn-mode-map
  (let ((map (copy-keymap lisp-mode-map)))
    (make-keymap)))

(add-to-list 'auto-mode-alist '("\\.fn\\'" . fn-mode))

(defvar fn-definers
  '("def" "defmacro" "defvar"))
(defvar fn-special-operators
  '("apply" "defmethod" "defclass" "case" "cond" "do" "if" "fn" "let" "get" "get-field" "set"
    "quote" "quasiquote" "unquote" "unquote-splice" "dollar-fn")
  "Special operators that have their names highlighted")
(defvar fn-constants
  '("$" "$0" "$1" "$2" "$3" "$4" "$5" "&" "true" "false" "null" "_"))

;; indentation rules for special operators
(defvar fn-operator-indent-alist
  '(("case" 4 cycle 2 4)
    ("cond" cycle 2 4)
    ("def" 4 . 2)
    ("defclass" 4 . 2)
    ("defmethod" 4 . 2)
    ("defmacro" 4 . 2)
    ("defvar" 4 . 2)
    ("do" . 2)
    ("fn" 4 . 2)
    ("let" 4 . 2)))

(defvar fn-font-lock-keywords
  (list
   ;; definers
   (list (concat
          "("
          (regexp-opt fn-definers t)
          "\\_>")
         '(1 font-lock-keyword-face))
   ;; definition names
   (list (concat
          "("
          (regexp-opt '("def" "defvar") t)
          ;; whitespace
          "\\_>[[:space:]]+"
          ;; symbol
          "\\_<\\(\\(?:\\s_\\|\\w\\)+\\)\\_>")
         '(2 font-lock-variable-name-face))
   ;; function & macro definitions
   (list (concat
          "("
          (regexp-opt '("def" "defmacro" "defvar") t)
          ;; whitespace and opening paren
          "\\_>[[:space:]]+("
          ;; symbol
          "\\_<\\(\\(?:\\s_\\|\\w\\)+\\)\\_>")
         '(2 font-lock-function-name-face))
   ;; method definition
   (list (concat
          "("
          (regexp-opt '("def" "defmethod") t)
          ;; whitespace and TWO opening parens
          "\\_>[[:space:]]+(("
          ;; symbol
          "\\_<\\(\\(?:\\s_\\|\\w\\)+\\)\\_>")
         '(2 font-lock-function-name-face))
   ;; other special operators
   (list (concat "("
                 (regexp-opt fn-special-operators t)
                 "\\_>")
         1 'font-lock-keyword-face)
   ;; special constants (True, False, etc)
   (cons (concat "\\_<"
                 (regexp-opt fn-constants t)
                 "\\_>")
         'font-lock-constant-face)
   ;; quoted symbols
   (list "\\('\\(?:\\sw\\|\\s_\\)+\\_>\\)"
         1 'font-lock-constant-face)
   ;; Symbols starting with uppercase characters (presumed types/constructors)
   (list "\\_<\\([[:upper:]]\\(?:\\sw\\|\\s_\\)*\\)\\_>"
         1 'font-lock-type-face)))

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
    (modify-syntax-entry ?\# "w" stab)
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
    (modify-syntax-entry ?\$ "'" stab)
    stab))


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

(defun fn--count-sexps-before (pos)
  "Count the number of sexps before pos."
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

(defun fn--compute-indent-column (cxt &optional offset)
  "Figure out how to indent a line using the given context. If
 offset is nil, indentation is done using built-in rules.
 Otherwise, the column is the operator column plus the offset."
  (if offset
      (+ offset (context-op-col cxt))
    ;; null offset means use standard indentation rules.
    (let ((ind (fn--find-indenter (context-delim cxt) (context-op cxt))))
      (if ind
          (+ (fn--indenter-offset ind (context-list-pos cxt))
             (context-delim-col cxt))
          (or (context-cadr-col cxt)
              (context-op-col cxt)
              (print cxt))))))

(defun fn--indenter-offset (indenter index)
  "Use an indenter to decide the offset of an element in an fn
 code list. Index is the index of the element in question.
 Returns an offset."
  (let* ((is-cycle (and (listp indenter)
                        (eq (car indenter) 'cycle)))
         (is-atom (or (atom indenter) is-cycle)))
    (cond 
     (is-atom
      (if is-cycle
          (nth (mod (+ index 1) (- (length indenter) 1))
               (cdr indenter))
        indenter))
     ((= index 0) 1)
     ((= index 1) (car indenter))
     (t (fn--indenter-offset (cdr indenter) (- index 1))))))

(defun fn--find-indenter (delim op)
  (cond
   ((null delim) 0)
   ((eql delim ?\()
    (let ((c (assoc op fn-operator-indent-alist 'equal)))
      (if c
          (cdr c)
        nil)))
   ((eql delim ?\[) 1)
   ((eql delim ?\{) '(cycle 1 3))
   ((eql delim ?\") nil)
   (t nil)))

;; nil value indicates previous indent was a standard lisp indent/custom indenter
;; this just cycles between 2 indent and standard indent
(defun fn--next-indent-offset ()
  (case fn--prev-indent-offset
    ((nil) 1)
    (t nil)))

(defun fn--indent-line (again)
  "Internal indentation function."
  (if again
      (setq fn--prev-indent-offset (fn--next-indent-offset))
    (setq fn--prev-indent-offset nil))
  (beginning-of-line)
  (let ((cxt (fn--current-context))
        (offset (when again fn--prev-indent-offset)))
    (indent-line-to (fn--compute-indent-column cxt offset))))

(defun fn-indent-line ()
  "Indent current line as fn code"
  (interactive "*")
  (fn--indent-line (and (eq last-command this-command)
                        (memq this-command fn-indent-cycle-commands))))

(defvar fn-indent-cycle-commands
  '(fn-indent-line indent-for-tab-command indent-according-to-mode)
  "commands which trigger indent cycling")

;; TODO: add more special cases (e.g. formatting when the operator is alone on
;; the first line, formatting let bindings, etc)
(defun fn-mode ()
  "Major mode for editing fn programming language source code."
  (interactive)
  (kill-all-local-variables)
  (set-syntax-table fn-mode-syntax-table)
  (use-local-map fn-mode-map)
  (set (make-local-variable 'font-lock-defaults)
       '(fn-font-lock-keywords nil nil))
  (set (make-local-variable 'fn--prev-indent-offset)
       nil)
  ;;(set (make-local-variable 'indent-line-function) 'fn-indent-line)
  (setq-local indent-line-function 'fn-indent-line)
  (setq major-mode 'fn-mode)
  (setq mode-name "fn")
  (setq-local comment-start "; ")
  (setq-local commend-end "")
  (run-hooks 'fn-mode-hook))
