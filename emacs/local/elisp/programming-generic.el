;;----------------------------------------------------------------------
;; programming.el
;;
;; programming configuration including templates,merging, highlighting,
;; completion etc.
;;----------------------------------------------------------------------
(require 'custom-key)

;;
;; major programming features
;;

;;----------------------------------------------------------------------
;; advanced modes
;;----------------------------------------------------------------------

;; this is totally broken from git

;; (require 'tree-sitter)
;; (require 'tree-sitter-langs)
;; (require 'tree-sitter-debug)
;; (require 'tree-sitter-query)

;; (require 'tree-sitter-hl) -- need to convert font-lock faces later

;;
;; some generic code editing stuff
;;

(defun toggle-comment-region ()
  "toggle-comment-region

   comment or uncomment the region
  "
  (interactive)
  (comment-or-uncomment-region (mark) (point)) )

(defun toggle-comment-buffer ()
  "toggle-comment-buffer

   comment or uncomment the region
  "
  (interactive)

  (mark-whole-buffer)
  (call-interactively 'toggle-comment-region) )

(defun programming-search-missing ()
  (interactive)
  (message "function signature to find is not specified"))

(defvar configure-programming-hook nil
  "hook so other programming tools can run after programming-mode-generic")

(defun programming-mode-generic ( &optional fn-search )
  "Enable my programming customizations for the buffer"

  ;; whitespace
  (setq indent-tabs-mode nil)
  (whitespace-mode)

  ;; run hooks for programming configuration
  (run-custom-hooks configure-programming-hook)

  ;; better return key for programming
  (local-set-key (kbd "<return>") 'newline-and-indent)

  (let
    ((fn-search (or fn-search 'programming-search-missing)))

    (custom-key-group "search" "s" nil
      ("f" . fn-search)
      ("g" . grep)
      ("r" . rgrep)
      ("o" . occur)) )

  (custom-key-group "code editing" "c" nil
    ("c" . toggle-comment-region)
    ("b" . toggle-comment-buffer)
    ("i" . indent-region)
    ("s" . sort-lines) )

  (add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p t)
  (ruler-mode)

  ;; (tree-sitter-mode)
  ;; (ts-fold-mode)

  )

(provide 'programming-generic)
