# Simple C mode for Emacs

Because whatever comes with Emacs is too slow.

This mode does not try to be as featureful as `c-mode`, `c++-mode`,
`cc-mode`, etc. Instead we are trying to implement a bare minimum
syntax highlighting and indentation to maintain high performance on
big files with long lines.

The goal is to be able to comfortably browse and modify the following files:
- [imgui](https://raw.githubusercontent.com/ocornut/imgui/fb7f6cab8c322731da336e553915e944bf386e62/imgui.h)
- [amalgamated sqlite3.c](https://raw.githubusercontent.com/IreneKnapp/direct-sqlite/a74cc50c735053c7c49c487a66e7756b524db883/cbits/sqlite3.c)
- [miniaudio.h](https://raw.githubusercontent.com/mackron/miniaudio/refs/heads/master/miniaudio.h)
- ...

Right now the only way to work with these files in Emacs is to use
`text-mode`. Which is actually a good evidence that Emacs itself can
handle such files! It's `c-mode` (and others) that cannot.

## Installing locally

Put [simpc-mode.el](./simpc-mode.el) to some folder `/path/to/simpc/`. Add this to your `.emacs`:

```el
;; Adding `/path/to/simpc` to load-path so `require` can find it
(add-to-list 'load-path "/path/to/simpc/")
;; Importing simpc-mode
(require 'simpc-mode)
;; Automatically enabling simpc-mode on files with extensions like .h, .c, .cpp, .hpp
(add-to-list 'auto-mode-alist '("\\.[hc]\\(pp\\)?\\'" . simpc-mode))
```

## Indentation

Right now the mode supports only very simple indentations based on the
analysing the previous non-empty line and its surrounding curly
braces. Anything more complicated is outside of the scope of the
project.

It is recommended to use an external formatter such as
[indent](https://www.gnu.org/software/indent/),
[astyle](http://astyle.sourceforge.net/),
[clang-format](https://clang.llvm.org/docs/ClangFormat.html), etc.

Here is how I use [astyle](http://astyle.sourceforge.net/):

```emacs-lisp
(defun astyle-buffer ()
  (interactive)
  (let ((saved-line-number (line-number-at-pos)))
    (shell-command-on-region
     (point-min)
     (point-max)
     "astyle --style=kr"
     nil
     t)
    (goto-line saved-line-number)))
```

Then I bind `astyle-buffer` to some key combination and invoke it
periodically to reformat the whole current buffer.
