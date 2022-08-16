;;----------------------------------------------------------------------
;; slime
;;
;; slime profile for common lisp coding
;;----------------------------------------------------------------------
(require 'custom-key)
(require 'borg-repl)
(require 'programming-generic)
(require 'mode-tools)
(require 'profile/common-lisp)

(defconst cl-repl-name (borg-repl/repl-name cl-lisp-name))

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

(grail-require profile/dwim-complete
  "slime"
  "dwim complete source creation"

  (defun profile/slime-candidates ()
    (car (slime-simple-completions "")))

  (defun profile/slime-source ()
    (dwim-complete/make-source "slime"
      'profile/slime-candidates
      'dwim-complete-replace-stem ))

  (unless (dwim-complete-mode-check-type cl-lisp-name "mode")
    (dwim-complete-mode-add-source cl-lisp-name (profile/slime-source))
    (dwim-complete-mode-add-type cl-lisp-name "mode")) )

(defun profile/slime-common-setup ()
  (grail-require profile/syntax-tools
    "emacs-lisp"
    "syntax"

    (profile/syntax-tools-mode-setup)
    (profile/syntax-tools-lisp) )

  (grail-require profile/dwim-complete
    "slime common setup"
    "enable dwim-tabe"

    (dwim-complete/set-mode cl-lisp-name)
    (dwim-complete/for-buffer) ) )

;;
;; repl buffer setup
;;

(defun profile/slime-repl-setup ()
  (profile/slime-common-setup)

  (turn-on-dwim-tab 'lisp-indent-line)

  (buffer-ring/add cl-repl-name)
  (buffer-ring/local-keybindings) )

(add-hook 'slime-connected-hook 'profile/slime-repl-setup t)

;;
;; integration into common lisp
;;

(defun profile/slime-mode-setup ()
  (profile/slime-common-setup)

  ;; turn on minor mode
  (slime-mode t)

  (dwim-tab-localize-context (dwim-tab-make-expander 'dwim-tab-stem-trigger 'slime-complete-symbol))

  (borg-repl/bind-repl cl-repl-name
    'slime
    'slime-eval-last-expression
    'slime-eval-region
    'slime-eval-buffer
    'slime-eval-defun)

  (borg-repl/bind-connect 'slime-connect)

  (borg-repl/bind-macro-expand 'slime-macroexpand-1) )

(add-hook 'lisp-mode-hook 'profile/slime-mode-setup t)

(provide 'grail/slime)

