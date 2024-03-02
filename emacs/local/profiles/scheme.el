;;----------------------------------------------------------------------
;; scheme
;;----------------------------------------------------------------------
(require 'custom-key)
(require 'borg-repl)
(require 'buffer-ring)
(require 'programming-generic)

(require 'geiser)
(require 'geiser-chicken)

(defconst scheme/mode-name "scheme")
(defconst scheme/repl-name (borg-repl/repl-name scheme/mode-name))

(defvar scheme-function-decl ".*(define.*")

(setq
  scheme-program-name "csi"
  geiser-chicken-binary "csi"
  geiser-active-implementations '(chicken))

(defun new-scheme-repl ()
  "new-scheme-repl

   switch to an existing or create a new scheme REPL
  "

  (interactive)
  (run-scheme scheme-program-name)
  (pop-to-buffer scheme-buffer))

(defun scheme-list-functions ()
  (interactive)
  (occur cl-function-decl))

(grail-require profile/dwim-complete
  "scheme"
  "initializing dwim-complete"

  (dwim-complete/setup-for-buffer scheme/mode-name
    (lambda ()
      (let
        ((symbols nil))

        (dwim-complete-build-helm-from-generator "scheme/symbols" symbols)) )) )

(defun mattie-scheme-setup ()
  (geiser-mode)

  (borg-repl/bind-repl scheme/repl-name
    'new-scheme-repl
    'scheme-send-last-sexp
    'scheme-send-region
    'scheme-load-file
    'scheme-send-definition)

  (programming-mode-generic 'scheme-list-functions)

  (buffer-ring/add scheme/mode-name)
  (buffer-ring/local-keybindings)

  (turn-on-dwim-tab 'lisp-indent-line))

(add-hook 'scheme-mode-hook 'mattie-scheme-setup)

(provide 'profile/scheme)




