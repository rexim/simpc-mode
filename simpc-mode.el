(require 'subr-x)

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
   `("# *[a-zA-Z0-9_]+" . font-lock-preprocessor-face)
   `("#.*include \\(\\(<\\|\"\\).*\\(>\\|\"\\)\\)" . (1 font-lock-string-face))
   `(,(regexp-opt (simpc-keywords) 'symbols) . font-lock-keyword-face)))

(defun simpc--space-prefix-len (line)
  (- (length line)
     (length (string-trim-left line))))

(defun simpc--previous-non-empty-line ()
  (save-excursion
    (forward-line -1)
    (while (and (not (bobp))
                (string-empty-p
                 (string-trim-right
                  (thing-at-point 'line t))))
      (forward-line -1))
    (thing-at-point 'line t)))

;;; TODO: wrong non-curly brace construction indentation
;;;    if ()
;;;        f();
;;;    else
;;;        g();
;;;        t();   // <---
;;;
;;;    while()
;;;        f();
;;;        g();   // <---
;;; TODO: case body inside of switch is not indented properly
;;; TODO: simpc-indent-line should keep the cursor at the old relative position
;;; TODO: indentation does not take into account parens `(` and `)`
;;; TODO: customizable indentation (amount of spaces, tabs, etc)
(defun simpc-indent-line ()
  (interactive)
  (beginning-of-line)
  (when (not (bobp))
    (indent-line-to
     (let ((cur-line (string-trim-right (thing-at-point 'line t)))
           (prev-line (string-trim-right (simpc--previous-non-empty-line)))
           (indent-len 4))
       (cond
        ((and (string-suffix-p "{" prev-line)
              (string-prefix-p "}" (string-trim-left cur-line)))
         (simpc--space-prefix-len prev-line))
        ((or (string-suffix-p "{" prev-line)
             (string-prefix-p "if " (string-trim-left prev-line))
             (string-prefix-p "if(" (string-trim-left prev-line))
             (string-prefix-p "while " (string-trim-left prev-line))
             (string-prefix-p "while(" (string-trim-left prev-line))
             (string= "else" (string-trim prev-line)))
         (+ (simpc--space-prefix-len prev-line) indent-len))
        ((or (string-prefix-p "}" (string-trim-left cur-line))
             (string= "else" (string-trim cur-line)))
         (max (- (simpc--space-prefix-len prev-line) indent-len) 0))
        (t (simpc--space-prefix-len prev-line)))))))

(define-derived-mode simpc-mode prog-mode "Simple C"
  "Simple major mode for editing C files."
  :syntax-table simpc-mode-syntax-table
  (setq-local font-lock-defaults '(simpc-font-lock-keywords))
  (setq-local indent-line-function 'simpc-indent-line)
  (setq-local comment-start "//"))
