;; -*-no-byte-compile: t; -*-

(defun ext-merge-with-ancestor ( remote-file local-file ancestor-file merge-file )
  (ediff-merge-files-with-ancestor remote-file local-file ancestor-file))

(defun ext-diff ( old new )
  (ediff-files old new))

(provide 'ext-merging)
