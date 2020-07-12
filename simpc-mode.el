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
   ;; TODO: file paths are not highlighted as strings in #include preprocessor directives
   ;;     #include <stdio.h>
   ;;     #include "foo.c"
   ;; TODO: string in #ifdef breaks preprocessor highlighting
   ;;     #ifdef ""
   `("#\\(.*\\)" . font-lock-preprocessor-face)
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

;;; TODO: no support for if-while-etc blocks without curly braces
;;;     if (...)
;;;        foo();
;;;     else
;;;        bar();
(defun simpc-indent-line ()
  (interactive)
  (beginning-of-line)
  (when (not (bobp))
    (indent-line-to
     (let ((cur-line (string-trim-right (thing-at-point 'line t)))
           (prev-line (string-trim-right (simpc--previous-non-empty-line))))
       (if (string-suffix-p "{" prev-line)
           (if (string-prefix-p "}" (string-trim-left cur-line))
               (simpc--space-prefix-len prev-line)
             (+ (simpc--space-prefix-len prev-line) 4))
         (if (string-prefix-p "}" (string-trim-left cur-line))
             (max (- (simpc--space-prefix-len prev-line) 4) 0)
           (simpc--space-prefix-len prev-line)))))))

(define-derived-mode simpc-mode prog-mode "Simple C"
  "Simple major mode for editing C files."
  :syntax-table simpc-mode-syntax-table
  (setq-local font-lock-defaults '(simpc-font-lock-keywords))
  (setq-local indent-line-function 'simpc-indent-line)
  (setq-local comment-start "//"))
