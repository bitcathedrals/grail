;; -*-no-byte-compile: t; -*-

;;
;; slime
;;
(require 'borg-repl)
(require 'programming-generic)
(require 'mode-tools)
(require 'profile/common-lisp)

(defconst slime/mode-name "slime")
(defconst slime/repl-name (borg-repl/repl-name slime/mode-name))

(require 'slime)

;;
;; mode defaults
;;

(setq
  slime-net-coding-system 'utf-8-unix

  inferior-lisp-program (executable-find "sbcl")

  slime-words-of-encouragement '("The name is Bond. James Bond."
                                  "These are your father's parentheses. Elegant weapons from a more civilized age."
                                  "We were on the edge of the desert when the Emacs took hold."
                                  "Mine says: Desert Eagle ... .50") )
;;
;; dwim setup
;;

(defun slime/slime-candidates ()
  (car (slime-simple-completions "")))

(grail-require profile/dwim-complete
  "slime"
  "dwim complete source creation"

  (dwim-complete/setup-for-buffer elisp/mode-name
    (lambda ()
      (dwim-complete-build-helm-from-generator "lisp" (slime/slime-candidates))) ) )

;;
;; repl buffer setup
;;

(defun slime/slime-repl-setup ()
  (turn-on-dwim-tab 'lisp-indent-line)

  (buffer-ring/add slime/repl-name)
  (buffer-ring/local-keybindings))

(add-hook 'slime-connected-hook 'slime/slime-repl-setup t)

;;
;; integration into common lisp
;;

(defun profile/slime-mode-setup ()
  ;; turn on minor mode
  (slime-mode t)

  (borg-repl/bind-repl slime/repl-name
    'slime
    'slime-eval-last-expression
    'slime-eval-region
    'slime-eval-buffer
    'slime-eval-defun)

  (borg-repl/bind-connect 'slime-connect)

  (borg-repl/bind-macro-expand 'slime-macroexpand-1))

(add-hook 'lisp-mode-hook 'profile/slime-mode-setup t)

;;
;; integration into common lisp
;;

(defun profile/slime-mode-setup ()
  ;; turn on minor mode
  (slime-mode t)

  (dwim-tab-localize-context
    (dwim-tab-make-expander
      'dwim-tab-stem-trigger
      'slime-complete-symbol))

  (borg-repl/bind-repl slime/repl-name
    'slime
    'slime-eval-last-expression
    'slime-eval-region
    'slime-eval-buffer
    'slime-eval-defun)

  (borg-repl/bind-connect 'slime-connect)

  (borg-repl/bind-macro-expand 'slime-macroexpand-1)

  (if (not (slime-connected-p))
    (slime)) )

(add-hook 'lisp-mode-hook 'profile/slime-mode-setup t)

(provide 'grail/slime)

