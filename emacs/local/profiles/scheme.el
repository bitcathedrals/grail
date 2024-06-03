;; -*-no-byte-compile: t; -*-

(require 'custom-key)
(require 'borg-repl)
(require 'programming-generic)

(require 'geiser)
(require 'geiser-chicken)
(require 'geiser-completion)

(require 'lsp-scheme)

(setq lsp-scheme-implementation "chicken")

(defconst scheme/mode-name "scheme")

(defvar scheme-function-decl ".*(define.*")

(setq
  scheme-program-name "csi"
  geiser-chicken-binary "csi"
  geiser-active-implementations '(chicken))

(defun profile/scheme-setup-repl ()
  (buffer-ring/add scheme/mode-name)
  (buffer-ring/local-keybindings))

(add-hook 'inferior-scheme-mode-hook #'profile/scheme-setup-repl)

(defun profile/scheme-buffer ()
  ;; defined in cmuscheme.el
  scheme-buffer)

(defun profile/scheme-repl ()
  "new-scheme-repl

   switch to an existing or create a new scheme REPL
  "
  (interactive)
  (let
    ((restore (current-buffer)))

    (run-scheme scheme-program-name)
    (pop-to-buffer (profile/scheme-buffer) 'display-buffer-pop-up-window)
    (other window 1)
    (switch-to-other-buffer restore)) )

(defun scheme-list-functions ()
  (interactive)
  (occur cl-function-decl))

(defun profile/scheme-setup ()
  (geiser-mode)

  (borg-repl/bind-repl
    'profile/scheme-repl
    'scheme-send-last-sexp
    'scheme-send-region
    'scheme-load-file
    'scheme-send-definition
    'profile/scheme-buffer)

  (programming-mode-generic 'scheme 'scheme-list-functions scheme/mode-name)

;;  (lsp-scheme)

  (turn-on-dwim-tab 'lisp-indent-line) )

(add-hook 'scheme-mode-hook 'profile/scheme-setup)

(provide 'profile/scheme)




