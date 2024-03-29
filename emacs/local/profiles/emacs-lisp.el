;; -*-no-byte-compile: t; -*-

(require 'custom-key)
(require 'borg-repl)
(require 'buffer-ring)
(require 'programming-generic)

(defconst elisp/mode-name "elisp-mode")
(defconst elisp/repl-name (borg-repl/repl-name elisp/mode-name))

;;
;; global settings
;;

(setq
  lisp-indent-offset 2)

;;
;; search functions
;;

(defun elisp-list-fn-signatures ()
  (interactive)
  (occur "(defun"))

;;
;; dwim tab completion backend
;;

(grail-require profile/dwim-complete
  "emacs-lisp requires dwim-complete"
  "building dwim-complete support for emacs lisp"

  (defun emacs-lisp-helm-generator ()
    (lambda ()
      (let
        ((functions nil)
         (variables nil))

        (mapatoms
          (lambda ( entry )
            (if (functionp entry)
              (setq functions (cons (symbol-name entry) functions))
              (setq variables (cons (symbol-name entry) variables))) )
          obarray)

        (list
          (dwim-complete-build-helm-from-generator "functions" functions)
          (dwim-complete-build-helm-from-generator "variables" variables)) )) ))

;;
;; borg-repl backend
;;

(defun elisp/repl-new ()
  (interactive)
  (let
    ((new-elisp-repl (get-buffer-create (concat "*" (generate-new-buffer-name "elisp/eval") "*")) ))

    (pop-to-buffer
      (if new-elisp-repl
        (with-current-buffer new-elisp-repl
          (emacs-lisp-mode)
          (current-buffer))
        (progn
          (message "profile/elisp: cannot create new scratch buffer")
          nil) )) ))

(defun emacs-lisp/profile ()
  (programming-mode-generic 'elisp-list-fn-signatures)

  (buffer-ring/add elisp/mode-name)
  (buffer-ring/local-keybindings)

  (borg-repl/bind-repl elisp/mode-name
    'elisp/repl-new
    'eval-last-sexp
    'eval-region
    'eval-buffer
    'eval-defun)

  (borg-repl/bind-macro-expand 'pp-macroexpand-last-sexp)

  (custom-key-group "elisp-debug" "d" nil
     ("d" . eval-defun))

  (grail-require profile/dwim-complete
    "emacs-lisp"
    "initializing dwim-complete"

    (dwim-complete/setup-for-buffer elisp/mode-name (emacs-lisp-helm-generator)))

  (turn-on-dwim-tab 'lisp-indent-line))

(add-hook 'emacs-lisp-mode-hook 'emacs-lisp/profile t)

(provide 'profile/emacs-lisp)
