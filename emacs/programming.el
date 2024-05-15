;; -*-no-byte-compile: t; -*-

(require 'merging)
(require 'ext-merging)

(require 'buffer-ring)

(require 'programming-generic)

(require 'borg-repl)

(setq vc-follow-symlinks t)

(require 'company)
;; (require 'company-tabnine)

;; Trigger completion immediately.
(setq
  company-idle-delay 0)

;; (add-to-list 'company-backends #'company-tabnine)

;; indentation

;; disable electric stuff to avoid problems with my more sophisticated
;; modes

(electric-indent-mode 0)
(setq-default
  tab-width 2)

;; programming packages not dependent on third party support

(use-grail-profiles 0 "code-highlighting")

;; higher level functionality

(use-grail-profiles 1 "emacs-lisp" "common-lisp" "scheme" "shell-scripting" "python")

;; advanced functionality

(use-grail-profiles 3 "slime")

;; version control

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

(defun python/mode-functions ()
  "python-mode-functions

   occur all the functions in a python mode buffer
  "
  (interactive)

  (occur "def.*") )

(require 'eglot)

(setq
  eldoc-documentation-strategy 'ignore
  global-eldoc-mode nil
  eldoc-documentation-functions nil)

;; (setq eglot-ignored-server-capabilities '(:completionProvider))

(add-to-list 'eglot-server-programs '(python-mode . ("pylsp")))

(defun tree-sitter-install-python ()
  (interactive)
  (call-interactively 'treesit-install-language-grammar
    "python"
    (concat (getenv "HOME") "/tools/local/libexec")))

(defconst python/mode-name "python")

(defun tree-sitter-for-python ()
  (interactive)

  (treesit-language-available-p 'python))

(defun python/mode-setup ()
  "python-mode-setup

   setup python-mode enhanced features.
  "
  (interactive)

  (eglot-ensure)
  (company-mode)

  (buffer-ring/local-keybindings python/mode-name)

  (programming-mode-generic 'python/mode-functions) )

(add-hook 'python-mode-hook 'python/mode-setup)
