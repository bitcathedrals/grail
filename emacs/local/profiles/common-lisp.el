;; -*-no-byte-compile: t; -*-

(require 'buffer-ring)
(require 'dwim-tab)
(require 'programming-generic)

(defvar cl-function-decl ".*(defun.*")
(defconst cl-lisp-name "cl")

(defun cl-list-functions ()
  (interactive)
  (occur cl-function-decl))

(require 'lisp-mode)

(setq
  auto-mode-alist (append '(("\\.cl$" . lisp-mode)
                            ("\\.lisp$" . lisp-mode)) auto-mode-alist))

(defun profile/cl-mode-setup ()
  (programming-mode-generic 'cl-list-functions)

  (turn-on-dwim-tab 'lisp-indent-line))

(add-hook 'lisp-mode-hook 'profile/cl-mode-setup)

(provide 'profile/common-lisp)
