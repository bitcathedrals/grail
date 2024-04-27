;; -*-no-byte-compile: t; -*-

;;----------------------------------------------------------------------
;; merging.el
;;
;; merging and diffing support (internal)
;;----------------------------------------------------------------------
(require 'diff)

;;----------------------------------------------------------------------
;; regular diff mode
;;----------------------------------------------------------------------

(setq
  diff-switches "-U3")                 ;; turn on standard context diffs,

(add-hook 'diff-mode-hook

  ;; when diff is called it will pop a window which is nice, but killing
  ;; the buffer did not get rid of the popped window , until now.

  (lambda ()
    (add-hook 'kill-buffer-hook 'rid-window t t))
  t)

;;----------------------------------------------------------------------
;;                          Ediff
;;----------------------------------------------------------------------

(require 'ediff)

(setq
  ediff-custom-diff-options "-U3"         ;; same for ediff

  ediff-split-window-function 'split-window-horizontally
  ediff-merge-split-window-function 'split-window-horizontally

  ediff-window-setup-function 'ediff-setup-windows-plain

  ediff-auto-refine t)

(defun merging-buffers-to-name ()
  (mapcar
    (lambda ( buffer-object )
      (buffer-name buffer-object))
    (buffer-list)) )

(defun merging-buffer-predicate-p ( buffer-name buffer-spec )
  (let*
    (( case-fold-search t )
     ( result (string-match buffer-spec buffer-name) ))

    (if (eq nil result)
      nil
      t)))

(defun merging-matching-buffers-by-name ( match-spec )
  (let
    (( match-list nil ))

    (mapc
      (lambda ( buffer-name )
        (when (eval `(or
                       ,@(mapcar
                           (lambda ( spec )
                             (merging-buffer-predicate-p buffer-name spec))
                           match-spec)))

          (setq match-list (cons buffer-name match-list)) ))
        (merging-buffers-to-name))
    match-list))

(defun merging-kill-all-listed ( kill-list )
  (mapc
    (lambda ( buffer-name )
      (kill-buffer buffer-name))
    kill-list) )

(defconst merging-ediff-interface-buffer-regex-list '( "\*.*Ediff.*\*"  ))

(defun merging-ediff-teardown-interface ( keep-buffer )
  (switch-to-buffer keep-buffer)
  (delete-other-windows)

  (merging-kill-all-listed
    (merging-matching-buffers-by-name merging-ediff-interface-buffer-regex-list)) )

(provide 'merging)
