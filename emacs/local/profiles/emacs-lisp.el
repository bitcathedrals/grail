;;
;; emacs-lisp.el - emacs lisp mode configuration
;;
;; configure emacs lisp with enhanced programming features
;;

(require 'custom-key)
(require 'borg-repl)
(require 'buffer-ring)
(require 'programming-generic)

(defconst elisp/mode-name major-mode)
(defconst elisp/repl-name (borg-repl/repl-name elisp/mode-name))

;;
;; global settings
;;

(setq
  lisp-indent-offset 2)

;;
;; key-binding search functions
;;

(defun elisp-list-fn-signatures ()
  (interactive)
  (occur "(defun"))


;;
;; walk the symbol tables
;;

(defun emacs-lisp-function-symbols ()
  (let
    (( name-list ))

    (mapatoms
      (lambda ( sym )
        (when (functionp sym)
          (setq name-list (cons (symbol-name sym) name-list ))) )
      obarray)

    name-list))

  (defun emacs-lisp-variable-symbols ()
    (let
      (( name-list ))

      (mapatoms
        (lambda ( sym )
          (unless (functionp sym)
            (setq name-list (cons (symbol-name sym) name-list ))) )
        obarray)

      name-list))


;;
;; dwim tab completion backend
;;

(grail-require profile/dwim-complete
  "emacs-lisp"
  "defining dwim-complete sources"

  (defun dwim-complete/emacs-lisp-fn-source ()
    (dwim-complete/make-source "functions"

      (lambda ()
        (emacs-lisp-function-symbols))

      'dwim-complete-replace-stem))


  (defun dwim-complete/emacs-lisp-var-source ()
    (dwim-complete/make-source "variables"

      (lambda ()
        (emacs-lisp-variable-symbols))

      'dwim-complete-replace-stem)) )

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

    (dwim-complete/mode-add
      elisp/mode-name
      (dwim-complete/emacs-lisp-fn-source))

    (dwim-complete/mode-add
      elisp/mode-name
      (dwim-complete/emacs-lisp-var-source))

    (dwim-complete/setup-for-buffer))

  (turn-on-dwim-tab 'lisp-indent-line))

(add-hook 'emacs-lisp-mode-hook 'emacs-lisp/profile t)

(provide 'profile/emacs-lisp)
