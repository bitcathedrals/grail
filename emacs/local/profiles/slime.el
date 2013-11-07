;;----------------------------------------------------------------------
;; slime
;;
;; slime profile for common lisp coding
;;----------------------------------------------------------------------

(grail-load 'slime     (grail-define-installer "slime"
                         "cvs"
                         ":pserver:anonymous:anonymous@common-lisp.net:/project/slime/cvsroot"))

(setq hyperspec-dir
  (grail-fetch-docs "hyperspec"
    (grail-define-installer "hyperspec" "tar:gz"
      "ftp://ftp.lispworks.com/pub/software_tools/reference/HyperSpec-7-0.tar.gz")
    1))

;;----------------------------------------------------------------------
;; SLIME
;;----------------------------------------------------------------------
(require 'slime)

(setq
  slime-net-coding-system 'utf-8-unix

  inferior-lisp-program "sbcl"

  slime-words-of-encouragement '("The name is Bond. James Bond."
                                  "These are your father's parentheses. Elegant weapons from a more civilized age."
                                  "We were on the edge of the desert when the Emacs took hold."
                                  "Mine says: Desert Eagle ... .50")
  common-lisp-hyperspec-root (concat "file://" hyperspec-dir "/"))

(add-hook 'slime-connected-hook
  (lambda ()
    (lisp-smart-parens-editing)

    (set-popup-browse-html-local)
    (configure-for-docs 'slime-hyperspec-lookup)

    (configure-for-buffer-ring "lisp-mode")

    (rename-buffer (generate-new-buffer-name "cl-repl")) )
  t)

(add-hook 'lisp-mode-hook
  (lambda ()
    (slime-mode t)

    (set-popup-browse-html-local)
    (configure-for-docs 'slime-hyperspec-lookup)

    (dwim-tab-localize-context 'slime-complete-symbol)

    (configure-for-evaluation
      'slime-eval-defun
      'slime-eval-last-expression
      'slime-eval-region
      'slime-eval-buffer) )
  t)

(add-hook 'inferior-lisp-mode-hook
  (lambda () (inferior-slime-mode t)))


