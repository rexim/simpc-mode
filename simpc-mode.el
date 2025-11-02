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
    ;; Treat <> as punctuation (needed to highlight C++ keywords
    ;; properly in template syntax)
    (modify-syntax-entry ?< "." table)
    (modify-syntax-entry ?> "." table)

    (modify-syntax-entry ?& "." table)
    (modify-syntax-entry ?% "." table)
    table))

(defun simpc-types ()
  '("char" "int" "long" "short" "void" "bool" "float" "double" "signed" "unsigned"
    "char16_t" "char32_t" "char8_t"
    "int8_t" "uint8_t" "int16_t" "uint16_t" "int32_t" "uint32_t" "int64_t" "uint64_t"
    "uintptr_t"
    "size_t"
    "va_list"))

(defun simpc-keywords ()
  '("auto" "break" "case" "const" "continue" "default" "do"
    "else" "enum" "extern" "for" "goto" "if" "register"
    "return"  "sizeof" "static" "struct" "switch" "typedef"
    "union"  "volatile" "while" "alignas" "alignof" "and"
    "and_eq" "asm" "atomic_cancel" "atomic_commit" "atomic_noexcept" "bitand"
    "bitor" "catch"  "class" "co_await"
    "co_return" "co_yield" "compl" "concept" "const_cast" "consteval" "constexpr"
    "constinit" "decltype" "delete" "dynamic_cast" "explicit" "export" "false"
    "friend" "inline" "mutable" "namespace" "new" "noexcept" "not" "not_eq"
    "nullptr" "operator" "or" "or_eq" "private" "protected" "public" "reflexpr"
    "reinterpret_cast" "requires" "static_assert" "static_cast" "synchronized"
    "template" "this" "thread_local" "throw" "true" "try" "typeid" "typename"
    "using" "virtual" "wchar_t" "xor" "xor_eq"))

(defun simpc-font-lock-keywords ()
  (list
   `("# *\\(warn\\|error\\)" . font-lock-warning-face)
   `("# *[#a-zA-Z0-9_]+" . font-lock-preprocessor-face)
   `("# *include\\(?:_next\\)?\\s-+\\(\\(<\\|\"\\).*\\(>\\|\"\\)\\)" . (1 font-lock-string-face))
   `("\\(?:enum\\|struct\\)\\s-+\\([a-zA-Z0-9_]+\\)" . (1 font-lock-type-face))
   `(,(regexp-opt (simpc-keywords) 'symbols) . font-lock-keyword-face)
   `(,(regexp-opt (simpc-types) 'symbols) . font-lock-type-face)))

(defun simpc--previous-non-empty-line ()
  "Returns either NIL when there is no such line or a pair (line . indentation)"
  (save-excursion
    ;; If you are on the first line, but not at the beginning of buffer (BOB) the `(bobp)`
    ;; function does not return `t`. So we have to move to the beginning of the line first.
    ;; TODO: feel free to suggest a better approach for checking BOB here.
    (move-beginning-of-line nil)
    (if (bobp)
        ;; If you are standing at the BOB, you by definition don't have a previous non-empty line.
        nil
      ;; Moving one line backwards because the current line is by definition is not
      ;; the previous non-empty line.
      (forward-line -1)
      ;; Keep moving backwards until we hit BOB or a non-empty line.
      (while (and (not (bobp))
                  (string-empty-p
                   (string-trim-right
                    (thing-at-point 'line t))))
        (forward-line -1))

      (if (string-empty-p
           (string-trim-right
            (thing-at-point 'line t)))
          ;; If after moving backwards for this long we still look at an empty
          ;; line we by definition didn't find the previous non-empty line.
          nil
        ;; We found the previous non-empty line!
        (cons (thing-at-point 'line t)
              (current-indentation))))))

(defun simpc--desired-indentation ()
  (let ((prev (simpc--previous-non-empty-line)))
    (if (not prev)
        (current-indentation)
      (let ((indent-len 4)
            (cur-line (string-trim-right (thing-at-point 'line t)))
            (prev-line (string-trim-right (car prev)))
            (prev-indent (cdr prev)))
        (cond
         ((string-match-p "^\\s-*switch\\s-*(.+)" prev-line)
          prev-indent)
         ((and (string-suffix-p "{" prev-line)
               (string-prefix-p "}" (string-trim-left cur-line)))
          prev-indent)
         ((string-suffix-p "{" prev-line)
          (+ prev-indent indent-len))
         ((string-prefix-p "}" (string-trim-left cur-line))
          (max (- prev-indent indent-len) 0))
         ((string-suffix-p ":" prev-line)
          (if (string-suffix-p ":" cur-line)
              prev-indent
            (+ prev-indent indent-len)))
         ((string-suffix-p ":" cur-line)
          (max (- prev-indent indent-len) 0))
         (t prev-indent))))))

;;; TODO: customizable indentation (amount of spaces, tabs, etc)
(defun simpc-indent-line ()
  (interactive)
  (when (not (bobp))
    (let* ((desired-indentation
            (simpc--desired-indentation))
           (n (max (- (current-column) (current-indentation)) 0)))
      (indent-line-to desired-indentation)
      (forward-char n))))

(define-derived-mode simpc-mode prog-mode "Simple C"
  "Simple major mode for editing C files."
  :syntax-table simpc-mode-syntax-table
  (setq-local font-lock-defaults '(simpc-font-lock-keywords))
  (setq-local indent-line-function 'simpc-indent-line)
  (setq-local comment-start "// "))

(provide 'simpc-mode)
