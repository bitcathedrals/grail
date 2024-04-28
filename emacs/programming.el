;; -*-no-byte-compile: t; -*-

(require 'merging)
(require 'ext-merging)

(require 'buffer-ring)

(require 'programming-generic)

(require 'borg-repl)

(setq vc-follow-symlinks t)


;; indentation

;; disable electric stuff to avoid problems with my more sophisticated
;; modes

(electric-indent-mode 0)

;; programming packages not dependent on third party support

(use-grail-profiles 0 "code-highlighting")

;; higher level functionality

(use-grail-profiles 1 "emacs-lisp" "common-lisp" "scheme" "shell-scripting")

;; advanced functionality

(use-grail-profiles 3 "slime")


;;                          version control

(require 'magit)

;; refresh after edit
(with-eval-after-load 'magit-mode
  (add-hook 'after-save-hook 'magit-after-save-refresh-status t))

(setq git-commit-style-convention-checks
      (remove 'non-empty-second-line git-commit-style-convention-checks))

(custom-key-group "magit git" "v" t
  ("v" . magit-status)
  ("l" . magit-log)

  ("e" . magit-ediff-dwim)

  ("a" . magit-stage)
  ("u" . magit-unstage)
  ("c" . magit-commit)
  ("&" . magit-commit-squash)
  ("x" . magit-commit-amend)

  ("+" . magit-ediff-show-staged)
  ("*" . magit-ediff-show-unstaged)
  ("s" . magit-ediff-show-stash)

  ("r" . magit-ediff-resolve-all)
  ("p" . magit-push))

;;----------------------------------------------------------------------
;; C/C++ common
;;----------------------------------------------------------------------

(setq auto-mode-alist (append '(("\\.c$"       . c-mode)
                                ("\\.cc$"      . c++-mode)
                                ("\\.cpp$"     . c++-mode)
                                ("\\.h$"       . c++-mode)
                                ("\\.scheme$"  . scheme-mode)) auto-mode-alist))

(defun c-mode-generic-setup ()
  (c-set-style "linux")                 ;; base off of linux style
  (setq c-basic-offset 2)               ;; tabs are 2 spaces

  (c-set-offset 'substatement-open '0)  ;; hanging braces

  ;; auto-hungry newline and whitespace delete
  (c-toggle-auto-hungry-state 1) )

(add-hook 'c-mode-common-hook 'c-mode-generic-setup t)

(defconst c-mode-name "C")

(defun c-mode-setup ()
  (programming-mode-generic)

  (buffer-ring/add c-mode-name)
  (buffer-ring/local-keybindings) )

(add-hook 'c-mode-hook 'c-mode-setup t)

(defconst c-mode-name "C++")

(defun c++-mode-setup ()
  (programming-mode-generic)

  (buffer-ring/add c++-mode-name)
  (buffer-ring/local-keybindings) )

(add-hook 'c++-mode-hook 'c++mode-setup t)

(defun shell-mode-functions ()
  "shell-mode-functions

   occur all the functions in a shell mode buffer
  "
  (interactive)
  (occur "function.*"))

(defun shell-mode-setup ()
  "shell-mode-setup

   setup shell mode with enhanced features
  "
  (interactive)
  (programming-mode-generic 'shell-mode-functions) )

(add-hook 'shell-mode-hook 'shell-mode-setup)

