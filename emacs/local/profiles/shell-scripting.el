;; -*-no-byte-compile: t; -*-

(require 'programming-generic)
(require 'dwim-tab)

(require 'generic-indent)

(defconst shell-function-regex "function")
(defconst profile/shell-name "shell")

(defun shell-list-fn-signatures ()
  (interactive)
  (occur shell-function-regex))

(defun profile/shell-mode-setup ()
  (programming-mode-generic 'shell-list-fn-signatures)

  (setq
    sh-indentation 2
    sh-basic-offset 2)

  (keymap-local-set "<return>" 'hard-electric-newline)

  (turn-on-dwim-tab) )

(add-hook 'sh-mode-hook 'profile/shell-mode-setup t)

(provide 'profile/shell-scripting)
