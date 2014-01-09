;;----------------------------------------------------------------------
;; common lisp
;;----------------------------------------------------------------------
(defvar cl-function-decl ".*(defun.*")

(defun cl-list-functions ()
  (interactive)
  (occur cl-function-decl))

(require 'lisp-mode)

;;----------------------------------------------------------------------
;; auto mode list additions
;;----------------------------------------------------------------------

(setq
  auto-mode-alist (append '(("\\.cl$" . lisp-mode)
                            ("\\.lisp$" . lisp-mode)) auto-mode-alist ))

(defun profile/common-lisp ()
  (configure-for-programming 'cl-list-functions "lisp-mode")

  (lisp-smart-parens-editing)

  (dwim-complete/for-buffer)

  (turn-on-dwim-tab 'common-lisp-indent-function))

(add-hook 'lisp-mode-hook 'profile/common-lisp t)
