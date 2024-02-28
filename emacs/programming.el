;;----------------------------------------------------------------------
;; programming.el
;;
;; programming configuration including templates,merging, highlighting,
;; completion etc.
;;----------------------------------------------------------------------
(require 'merging)
(require 'ext-merging)

(require 'buffer-ring)

(require 'programming-generic)

;;----------------------------------------------------------------------
;; indentation
;;----------------------------------------------------------------------

;; disable electric stuff to avoid problems with my more sophisticated
;; modes

(electric-indent-mode 0)

;;----------------------------------------------------------------------
;; programming packages not dependent on third party support
;;----------------------------------------------------------------------

;; re-usable programming modules

;; (use-grail-profiles 0 "code-formatting")

;; higher level functionality

(use-grail-profiles 0 "code-highlighting")

(use-grail-profiles 1 "emacs-lisp" "common-lisp" "shell-scripting")

;; advanced functionality

(use-grail-profiles 3 "slime")

;;----------------------------------------------------------------------
;;                          version control
;;----------------------------------------------------------------------
(require 'magit)

;; refresh after edit
(with-eval-after-load 'magit-mode
  (add-hook 'after-save-hook 'magit-after-save-refresh-status t))

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
;;                          misc tools
;;----------------------------------------------------------------------

(setq                     ;; cmd window + src
  gdb-show-main t)

(which-function-mode)

;;----------------------------------------------------------------------
;; C/C++ common
;;----------------------------------------------------------------------

(setq auto-mode-alist (append '(("\\.c$"       . c-mode)
                                ("\\.cc$"      . c++-mode)
                                ("\\.cpp$"     . c++-mode)
                                ("\\.h$"       . c++-mode)
                                 ) auto-mode-alist ))

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


