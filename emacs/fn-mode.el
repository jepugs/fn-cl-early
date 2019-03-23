(provide 'fn-mode)

(require 'lisp-mode)

(defvar fn-mode-hook nil)

(defvar fn-mode-map
  (let ((map (copy-keymap lisp-mode-map)))
    (make-keymap)))

(add-to-list 'auto-mode-alist '("\\.fn\\'" . fn-mode))

(defvar fn-definers
  '("def" "def*" "defdata" "defimpl" "defmacro" "defn"
    "defproto" "defvar")
  "Definer forms have syntax highlighting on the operator and the
 first argument. Their bodies are indented two spaces past the
 previous one.")
(defvar fn-special-operators
  '("@" "and" "case" "cond" "do" "if" "fn" "let" "new" "or" "progn" "set!")
  "Special operators that have their names highlighted")
(defvar fn-constants
  '("$" "$0" "$1" "$2" "$3" "$4" "$5" "&" "true" "false" "null" "_"))

;; indentation specifiers for certain operators
(defvar fn-operator-indent-alist
  '(("case" 1 4 cycle 2 4)
    ("cond" 1 cycle 2 4)
    ("def" 1 cycle 2 4)
    ("def*" 1 cycle 2 4)
    ("defdata" 1 4 4 . 2)
    ("defimpl" 1 4 . 2)
    ("defmacro" 1 4 . 2)
    ("defn" 1 4 4 . 2)
    ("defproto" 1 4 4 . 2)
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


(defun fn-read-next-sexp ()
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

(defun fn-count-sexps-before (pos)
  "Count the number of sexps before pos."
  (save-excursion
    (do ((n 0 (+ n 1)))
        ((> (point) pos) (- n 1))
      (condition-case nil
          (forward-sexp)
        ;; assume this error means we're at the end of the list
        (error (return n))))))

(defun fn-surrounding-list ()
  "Find out information about the list we're currently in.
 Returns a list (DELIM-CHAR DELIM-COL OPERATOR NUMBER) or nil if
 we're on the top level. DELIM-CHAR and DELIM-COL are the
 delimiter that opened this list and its column number. OPERATOR
 is a string, the first sexp in the list. NUMBER is the
 index (from 0) in the list of the next sexp after the mark (even
 if it's not there yet)."
  ;; first, check if we're inside quotes
  (if (nth 3 (syntax-ppss))
      '(?\" 0 "" 0)
    (let ((d-ch nil)
          (d-col 0)
          (op "")
          (n 0)
          (start (point)))
      (save-excursion
        (condition-case nil
            (progn
              (backward-up-list)
              (setq d-ch (char-after))
              (setq d-col (current-column))
              (forward-char)
              (setq op (fn-read-next-sexp))
              (setq n (fn-count-sexps-before start)))
          ;; assume this is a scan error, meaning we're at the top level
          (error
           (setq d-ch nil)
           (setq d-col 0)
           (setq op "")
           (setq n 0))))
      (list d-ch d-col op n))))


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

(defun fn-indent-line ()
  "Indent current line as fn code"
  (interactive)
  (let ((start (point)))
    (beginning-of-line)
    (let* ((context (fn-surrounding-list))
           (ind (fn-lookup-indenter (car context) (caddr context)))
           (off (fn-find-offset ind (cadddr context))))
      (goto-char start)
      (cond
       ((null off) ) ;do nothing
       ((numberp off)
        (indent-line-to (+ off (cadr context))))
       (t (error "fn-indent-line: non-numeric indenter"))))))

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
  (set (make-local-variable 'indent-line-function) 'fn-indent-line)
  (setq major-mode 'fn-mode)
  (setq mode-name "fn")
  (run-hooks 'fn-mode-hook))
