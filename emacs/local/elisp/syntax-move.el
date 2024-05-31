;; -*-no-byte-compile: t; -*-

(require 'puni)

(make-variable-buffer-local 'syntax-move/enabled)
(make-variable-buffer-local 'syntax-move/forward)
(make-variable-buffer-local 'syntax-move/backward)
(make-variable-buffer-local 'sytax-move/up)
(make-variable-buffer-local 'syntax-move/down)
(make-variable-buffer-local 'syntax-move/mark)

(defun syntax-move/error-msg ( msg &rest info )
  (message "syntax-move Error: %s %s" msg
    (if info
      (concat "[ " (string-join (mapcar
                                 (lambda (x)
                                   (if (stringp x)
                                     x
                                     (pp-to-string x)))
                                 info) ", ") " ]")
      "")))

(defun syntax-move/invoke-binding ( binding )
  (if (symbol-function binding)
    (if (commandp binding)
      (let
        ((current-prefix-arg nil))
        (call-interactively binding))
      (funcall binding) )
    (syntax-move/error-msg  "syntax-move: no binding for - " (symbol-name binding)) ))


(defun syntax-move/enabled ()
  "syntax-move/enabled

   return non-nil if syntax-move is enabled in the buffer.
  "
  (if (boundp 'syntax-move/enabled)) )

(defun syntax-move/forward ()
  "syntax-move/forward

   move forward by syntax
  "
  (interactive)
  (syntax-move/invoke-binding 'syntax-move/forward))

(defun syntax-move/backward ()
  "syntax-move/backward

   connect to a running repl.
  "
  (interactive)
  (syntax-move/invoke-binding 'syntax-move/backward))

(defun syntax-move/up ()
  "syntax-move/up

   move up the syntax tree.
  "
  (interactive)
  (syntax-move/invoke-binding 'syntax-move/up))

(defun syntax-move/down ()
  "syntax-move/down

   move down the syntax tree
  "
  (interactive)
  (syntax-move/invoke-binding 'syntax-move/down))

(defun syntax-move/mark ()
  "syntax-move/mark

   mark the current syntax, moving upward.
  "
  (interactive)
  (syntax-move/invoke-binding 'syntax-move/mark))

(defun syntax-move/bind-lang (lang)
  "syntax-move/bind-lang

   bind syntax movement functions for LANG where lang
   is a symbol with the language name.
  "
  (fset 'syntax-move/enabled t)

  (if (and
        lang
        (treesit-language-available-p lang))
    (message "tree-sitter support not available for lang %s" (symbol-name lang))
    (progn
      (fset 'syntax-move/forward  'puni-forward-sexp)
      (fset 'syntax-move/backward 'puni-backward-sexp)
      (fset 'syntax-move/up       'puni-backward-sexp-or-up-list)
      (fset 'syntax-move/down     'down-list)
      (fset 'syntax-move/mark     'puni-expand-region)) )

  (keymap-local-set "C-c <right>" 'syntax-move/forward)
  (keymap-local-set "C-c <left>"  'syntax-move/backward)
  (keymap-local-set "C-c <up>"    'syntax-move/up)
  (keymap-local-set "C-c <down>"  'syntax-move/down)
  (keymap-local-set "C-c m"       'syntax-move/mark) )

(provide 'syntax-move)
