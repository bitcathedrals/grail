;; -*-no-byte-compile: t; -*-

(require 'buffer-ring)

(require 'programming-generic)
(require 'borg-repl)

(require 'grail-diff)

(grail-diff-configure)

;;
;; language support
;;

(require 'treesit)

(setq treesit-font-lock-level 4)

;; disable treesit entirely until font lock issues are resolved
(setq use-tree-sitter nil)

(defun use-tree-sitter (lang)
  (and
   (eq use-tree-sitter t)
   (treesit-language-available-p lang)))

(require 'eglot)

(require 'lsp-mode)
(require 'lsp-lens)
(require 'lsp-modeline)
(require 'lsp-headerline)

(add-hook 'lsp-mode-hook #'lsp-lens-mode)
(setq lsp-signature-render-documentation nil)

(require 'helm-lsp)

;; Trigger completion immediately.
(setq company-idle-delay 0)

;; disable electric stuff to avoid problems with my more sophisticated
;; modes

(electric-indent-mode 0)

(setq-default tab-width 4)

;; programming packages not dependent on third party support

;; "code-formatting" - filladapt is orphaned
(use-grail-profiles 0 "code-highlighting")

;; higher level functionality

(use-grail-profiles 1 "emacs-lisp" "common-lisp" "scheme" "shell-scripting" "python")

;; advanced functionality

(use-grail-profiles 3 "slime" "git")

(setq vc-follow-symlinks t)

;;
;; C/C++
;;

(defun makefile-tuning ()
  (setq indent-tabs-mode t))

(add-hook 'makefile-mode-hook 'makefile-tuning)

(require 'cc-mode)


(setq auto-mode-alist (if (use-tree-sitter 'c)
                        (append '(("\\.c\\'" . c-ts-mode)) auto-mode-alist)
                        (append '(("\\.c\\'" . c-mode)) auto-mode-alist)))

(setq auto-mode-alist (if (use-tree-sitter 'cpp)
                        (append '(("\\.cc\\'"      . c++-ts-mode)
                                  ("\\.cpp\\'"     . c++-ts-mode)
                                  ("\\.h\\'"       . c++-ts-mode)) auto-mode-alist)
                        (append '(("\\.cc\\'"      . c++-mode)
                                  ("\\.cpp\\'"     . c++-mode)
                                  ("\\.h\\'"       . c++-mode)) auto-mode-alist)))

(defun cc-syntax-offset ()
  (interactive)

  (c-set-offset 'substatement-open 0)
  (c-set-offset 'defun-open 0))

(add-hook 'c-mode-common-hook 'cc-syntax-offset)

(defun c-mode-generic-setup ()
  (setq
    c-basic-offset 4
    indent-tabs-mode nil)

  (setq c-default-style '((c++-mode . "stroustrup")
                          (c-mode . "linux")
                          (other . "k&r")))

  (c-toggle-auto-hungry-state 1))

(add-hook 'c-mode-common-hook 'c-mode-generic-setup t)

(defconst c-mode-name "C")

(defun c-mode-setup ()
  (programming-mode-generic 'c))

 (add-hook 'c-mode-hook 'c-mode-setup t)

(defconst c-mode-name "C++")

(defun c++-mode-setup ()
  (programming-mode-generic 'c++))

(add-hook 'c++-mode-hook 'c++-mode-setup t)

;;
;; bash mode
;;

(require 'sh-script)

(setq auto-mode-alist (append
                        (if (use-tree-sitter 'bash)
                          '(("\\.sh\\'" . bash-ts-mode))
                          '(("\\.sh\\'" . bash-mode)))
                        auto-mode-alist))

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
  (programming-mode-generic 'shell 'shell-mode-functions))

(add-hook 'shell-mode-hook 'shell-mode-setup)

;;
;; python
;;

(setq auto-mode-alist (append
                        (if (use-tree-sitter 'python)
                          '(("\\.py\\'" . python-ts-mode))
                          '(("\\.py\\'" . python-mode)))
                        auto-mode-alist))

(defun python/mode-functions ()
  "python-mode-functions

   occur all the functions in a python mode buffer
  "
  (interactive)

  (occur "def.*"))

(setq
  eldoc-documentation-strategy 'ignore
  global-eldoc-mode nil
  eldoc-documentation-functions nil)

;; (setq eglot-ignored-server-capabilities '(:completionProvider))

(add-to-list 'eglot-server-programs '(python-mode . ("pylsp")))

(defconst python/mode-name "python")

(defun python/mode-setup ()
  "python-mode-setup

   setup python-mode enhanced features.
  "
  (interactive)

  (eglot-ensure)
  (company-mode)

  (programming-mode-generic 'python 'python/mode-functions))

(add-hook
  (if (treesit-language-available-p 'python)
    'python-ts-mode-hook
    'python-mode-hook)

  'python/mode-setup)


;;
;; java
;;

(setq auto-mode-alist (append
                        (if (use-tree-sitter 'java)
                          '(("\\.java\\'" . java-ts-mode))
                          '(("\\.java\\'" . java-mode)))
                        auto-mode-alist))
;;
;; scheme
;;

(setq auto-mode-alist (append '(("\\.scheme\\'"  . scheme-mode)) auto-mode-alist))

;;
;; html
;;

(require 'sgml-mode)

(defconst html-mode-config/name "html")

(setq auto-mode-alist (append
                        (if (use-tree-sitter 'html)
                          '(("\\.html\\'" . html-ts-mode))
                          '(("\\.html\\'" . html-mode)))
                        auto-mode-alist))

(defun html-mode/configuration ()
;;  (company-mode)
;;  (setq company-backends (cons 'company-capf company-backends))

  (programming-mode-generic 'html nil html-mode-config/name)

  (dwim-tab-make-expander 'dwim-tab/after-word 'company-complete))

(add-hook 'html-mode-hook 'html-mode/configuration)
