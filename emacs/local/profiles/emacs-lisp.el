;; -*-no-byte-compile: t; -*-

(require 'custom-key)
(require 'borg-repl)
(require 'buffer-ring)
(require 'programming-generic)

(defconst elisp/mode-name "elisp-mode")

;;
;; global settings
;;

(setq
  lisp-indent-offset 2)

;;
;; search functions
;;

(defun emacs-lisp/buffer-functions ()
  "emacs-lisp/buffer-functions

   use occur to find all function definitions
  "
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

            (if (functionp entry)
              (setq functions (cons (symbol-name entry) functions))
              (setq variables (cons (symbol-name entry) variables))) )
          obarray)

        (list
          (dwim-complete-build-helm-from-generator "functions" functions)
          (dwim-complete-build-helm-from-generator "variables" variables)) )) )

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
  (programming-mode-generic 'emacs-lisp/buffer-functions)

  (borg-repl/bind-repl
    'elisp/repl-new
    'eval-last-sexp
    'eval-region
    'eval-buffer
    'eval-defun
    nil)

  (borg-repl/bind-macro-expand 'pp-macroexpand-last-sexp)

  (custom-key-group "elisp-debug" "d" nil
     ("d" . eval-defun))

  (grail-require profile/dwim-complete
    "emacs-lisp"
    "initializing dwim-complete"

    (dwim-complete/setup-for-buffer elisp/mode-name (emacs-lisp-helm-generator)))

  (turn-on-dwim-tab 'lisp-indent-line))

(add-hook 'emacs-lisp-mode-hook 'emacs-lisp/profile)

(provide 'profile/emacs-lisp)
