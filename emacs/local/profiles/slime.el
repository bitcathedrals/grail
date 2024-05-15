;; -*-no-byte-compile: t; -*-

;;
;; slime
;;
(require 'borg-repl)
(require 'programming-generic)
(require 'mode-tools)
(require 'profile/common-lisp)

(defconst slime/mode-name "slime")
(defconst slime/file-mode-name "lisp")

(require 'slime)

;;
;; mode defaults
;;

(setq
  slime-net-coding-system 'utf-8-unix

  inferior-lisp-program (executable-find "sbcl")

  slime-words-of-encouragement words-of-encouragement)

(defun slime/slime-candidates ()
  (slime-simple-completions ""))

;;
;; repl buffer setup
;;

(defun slime/slime-repl-setup ()
  (turn-on-dwim-tab 'lisp-indent-line)

  (buffer-ring/add slime/mode-name)
  (buffer-ring/local-keybindings)

  (dwim-complete/setup-for-buffer slime/mode-name
    (dwim-complete/make-source "slime: "
      (lambda ()
        (slime/slime-candidates)) )) )

(add-hook 'slime-connected-hook 'slime/slime-repl-setup t)

;;
;; integration into common lisp
;;

(defun profile/slime-lisp-setup ()
  (interactive)

  (dwim-complete/setup-for-buffer slime/mode-name
    (dwim-complete/make-source "slime: "
      (lambda ()
        (slime/slime-candidates))) )

  (if (not (slime-connected-p))
    (slime))

  (slime-mode t)

  (borg-repl/bind-repl
    'slime
    'slime-eval-last-expression
    'slime-eval-region
    'slime-eval-buffer
    'slime-eval-defun
    nil
    nil)

  (borg-repl/bind-connect 'slime-connect)

  (borg-repl/bind-macro-expand 'slime-macroexpand-1))

(add-hook 'lisp-mode-hook 'profile/slime-lisp-setup)

(provide 'profile/slime)

