;; -*-no-byte-compile: t; -*-

(require 'programming-generic)
(require 'dwim-tab)

(defconst shell-function-regex "^function")
(defconst profile/shell-name "shell")

(defun shell-list-fn-signatures ()
  (interactive)
  (occur shell-function-regex))

(defun profile/shell-mode-setup ()
  (programming-mode-generic 'shell 'shell-list-fn-signatures)

  (setq
    sh-indentation 2
    sh-basic-offset 2))

(add-hook
  (if (treesit-language-available-p 'bash)
    'bash-ts-mode-hook
    'bash-mode-hook)

  'profile/shell-mode-setup)

(provide 'profile/shell-scripting)
