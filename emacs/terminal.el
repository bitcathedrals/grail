;; -*-no-byte-compile: t; -*-

;;----------------------------------------------------------------------
;; enhanced terminal
;;----------------------------------------------------------------------

(require 'term)
(require 'buffer-ring)
(require 'custom-key)

(defconst user-terminal-name "shell")

;;----------------------------------------------------------------------
;;                 IPC shell:  comint/term mode
;;----------------------------------------------------------------------

(defadvice term (after terminal/after-hook)
  (proc-close-on-exit/window))

(ad-activate 'term)

(setq explicit-shell-file-name "zsh"
      explicit-zsh-args '("-i"))

(defun user-terminal-ring ()
  (buffer-ring/add user-terminal-name)
  (buffer-ring/local-keybindings) )

(defadvice term (after terminal-profile/term-bindings)
  (user-terminal-ring))

(defadvice ansi-term (after terminal-profile/ansi-bindings)
  (user-terminal-ring))

(ad-activate 'term)
(ad-activate 'ansi-term)

(defun full-term ()
  "run a full terminal in full window C-u or (default) split window"
  (interactive)

  (split-window-below)
  (other-window 1)

  (term explicit-shell-file-name))

(defun shell-term ( prefix &optional command )
  "run a local shell in full window C-u or (default) split window"
  (interactive "P")

  (unless (and prefix (equal (car prefix) 4))
    (split-window-horizontally)
    (other-window 1))

  (ansi-term
    (if command
      command
      terminal-profile-local-shell)) )

(custom-key-group "execute" "x" t
    ("t" . full-term)
    ("s" . shell-term)
    ("c" . shell-command))

;; setup shell-mode in case I use it
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(add-to-list 'comint-output-filter-functions 'ansi-color-process-output)
