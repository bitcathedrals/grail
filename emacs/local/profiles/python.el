;; -*-no-byte-compile: t; -*-

(defconst python/mode-name "python")

(defun profile/python-setup-dwim-tab ()
  (interactive)
  (dwim-tab-localize-context (dwim-tab-make-expander 'dwim-tab/word-trigger 'python-completion-at-point)) )

(add-hook 'python-mode-hook 'profile/python-setup-dwim-tab)

(provide 'profile/python)
