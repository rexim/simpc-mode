(defvar simpc-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; C/C++ style comments
	(modify-syntax-entry ?/ ". 124b" table)
	(modify-syntax-entry ?* ". 23" table)
	(modify-syntax-entry ?\n "> b" table)
    ;; Preprocessor stuff?
    (modify-syntax-entry ?# "." table)
    ;; Chars are the same as strings
    (modify-syntax-entry ?' "\"" table)
    table))

(defun simpc-keywords ()
  '("auto" "break" "case" "char" "const" "continue" "default" "do" "double"
    "else" "enum" "extern" "float" "for" "goto" "if" "int" "long" "register"
    "return" "short" "signed" "sizeof" "static" "struct" "switch" "typedef"
    "union" "unsigned" "void" "volatile" "while"))

(defun simpc-font-lock-keywords ()
  (list
   `("#\\(.*\\)" . font-lock-preprocessor-face)
   `(,(regexp-opt (simpc-keywords)) . font-lock-keyword-face)))

(defun simpc-curly-nested-level ()
  (let ((level 0))
    (save-excursion
      (while (not (bobp))
        (cond
         ((= (following-char) ?{) (setq level (1+ level))))
        (forward-char -1)))
    level))

(defun simpc-indent-line ()
  (interactive)
  (beginning-of-line)
  (let ((cur-indent 0))
    (indent-line-to (* 4 (simpc-curly-nested-level)))))

(define-derived-mode simpc-mode prog-mode "Simple C"
  "Simple major mode for editing C files."
  :syntax-table simpc-mode-syntax-table
  (setq-local font-lock-defaults '(simpc-font-lock-keywords))
  (setq-local indent-line-function 'simpc-indent-line)
  (setq-local comment-start "//"))
