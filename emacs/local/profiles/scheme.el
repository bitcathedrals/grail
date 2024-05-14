;; -*-no-byte-compile: t; -*-

(require 'custom-key)
(require 'borg-repl)
(require 'buffer-ring)
(require 'programming-generic)

(require 'geiser)
(require 'geiser-chicken)
(require 'geiser-completion)

(defconst scheme/mode-name "scheme")
(defconst scheme/repl-name (borg-repl/repl-name scheme/mode-name))

(defvar scheme-function-decl ".*(define.*")

(setq
  scheme-program-name "csi"
  geiser-chicken-binary "csi"
  geiser-active-implementations '(chicken))

(defun profile/scheme-repl ()
  "new-scheme-repl

   switch to an existing or create a new scheme REPL
  "
  (interactive)
  (let
    ((restore (current-buffer)))

    (run-scheme scheme-program-name)
    (pop-to-buffer scheme-buffer 'display-buffer-pop-up-window)
    (other window 1)
    (switch-to-other-buffer restore)) )

(defun scheme-list-functions ()
  (interactive)
  (occur cl-function-decl))

(grail-require profile/dwim-complete
  "scheme"
  "initializing dwim-complete"

  (dwim-complete/setup-for-buffer scheme/mode-name
    (lambda ()
      (dwim-complete-build-helm-from-generator "scheme/symbols" (geiser-completion--symbol-list))) ) )

(defun profile/scheme-setup ()
  (geiser-mode)

  (borg-repl/bind-repl scheme/repl-name
    'profile/scheme-repl
    'scheme-send-last-sexp
    'scheme-send-region
    'scheme-load-file
    'scheme-send-definition
    nil
    nil)

  (programming-mode-generic 'scheme-list-functions)

  (buffer-ring/add scheme/mode-name)
  (buffer-ring/local-keybindings)

  (turn-on-dwim-tab 'lisp-indent-line) )

;; causes infinite loop

;; (defun profile/auto-launch-scheme ()
;;   (if (not (bufferp scheme-buffer))
;;     (pop-to-buffer (scheme-proc) 'display-buffer-pop-up-window)) )

(add-hook 'scheme-mode-hook 'profile/scheme-setup)

(provide 'profile/scheme)




