(provide 'fn-mode)

(defvar fn-mode-hook nil)

(defvar fn-mode-map
  (let ((map (copy-keymap lisp-mode-map)))
    (make-keymap)))

(add-to-list 'auto-mode-alist '("\\.fn\\'" . fn-mode))

;; definers highlight their first argument
(defvar fn-definers
  '("def" "def*" "defdata" "defimpl" "defmacro" "defn"
    "defproto" "defvar"))
(defvar fn-special-operators
  '("@" "and" "case" "cond" "do" "if" "fn" "let" "new" "or" "progn" "set!"))
(defvar fn-constants
  '("$" "$0" "$1" "$2" "$3" "$4" "$5" "&" "true" "false" "null" "_"))

(defvar fn-font-lock-keywords
  (list
   ;; definers
   (list (concat "("
                 (regexp-opt fn-definers t)
                 "\\_>\\(?:\\s-\\|\n\\)+\\(\\_<\\(?:\\s_\\|\\sw\\)+\\_>\\)?")
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
    ;; symbol characters
    (modify-syntax-entry ?- "_" stab)
    (modify-syntax-entry ?_ "_" stab)
    (modify-syntax-entry ?@ "w" stab)
    (modify-syntax-entry ?: "_" stab)
    (modify-syntax-entry ?! "w" stab)
    (modify-syntax-entry ?* "w" stab)
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

(defun fn-mode ()
  "Major mode for editing fn programming language source code."
  (interactive)
  (kill-all-local-variables)
  (set-syntax-table fn-mode-syntax-table)
  (use-local-map fn-mode-map)
  (set (make-local-variable 'font-lock-defaults)
       '(fn-font-lock-keywords nil nil))
  (setq major-mode 'fn-mode)
  (setq mode-name "fn")
  (run-hooks 'fn-mode-hook))
