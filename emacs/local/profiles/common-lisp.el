;; -*-no-byte-compile: t; -*-

;;
;; common lisp profile
;;
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
                            ("\\.lisp$" . lisp-mode)) auto-mode-alist ))

(defun profile/cl-mode-setup ()
  (programming-mode-generic 'cl-list-functions)

  (buffer-ring/add cl-lisp-name)
  (buffer-ring/local-keybindings)

  (grail-require profile/dwim-complete
    "common lisp"
    "initializing dwim-complete"

    (dwim-complete/setup-for-buffer cl-lisp-name))

  (turn-on-dwim-tab 'lisp-indent-line))

(add-hook 'lisp-mode-hook 'profile/cl-mode-setup t)

(provide 'profile/common-lisp)
