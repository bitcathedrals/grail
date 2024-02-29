;;----------------------------------------------------------------------
;; ext-merging
;;
;; external diff and merge support
;;----------------------------------------------------------------------
(defun ext-merge-write-merged-buffer ()
  (let
    ((file ediff-merge-store-file))

    (set-buffer ediff-buffer-C)

    (write-region (point-min) (point-max) file)
      (message "Merge buffer saved in: %s" file)

      (set-buffer-modified-p nil)
      (sit-for 1)))

(defun ext-merge-write-on-exit ()
  (add-hook 'ediff-quit-merge-hook 'ext-merge-write-merged-buffer t t))

(defun git-merge-with-ancestor ( local-file remote-file ancestor-file merge-file )
  (ediff-merge-files-with-ancestor
    local-file remote-file ancestor-file (list 'ext-merge-write-merged-buffer) merge-file))

(defun ext-merge-diff ( old new )
  (ediff-files old new))

(provide 'ext-merging)
