;; -*-no-byte-compile: t; -*-

(defun ext-merge-with-ancestor (current-file changed-file ancestor-file merge-file)
  (ediff-merge-files-with-ancestor remote-file local-file ancestor-file))

(defun ext-diff (base-file changed-file)
  "ext-diff BASE-FILE CHANGED-FILE

   diff CHANGED-FILE against changes in CHANGE-FILE
  "
  (ediff-files base-file changed-file))

(defun ext-diff-ancestor (left-file right-file ancestor-file)
  "ext-diff LEFT-FILE RIGHT-FILE ANCESTOR-FILE

   diff LEFT-FILE and RIGHT-FILE against ANCESTOR-FILE
  "
  (ediff-files3 left-file right-file ancestor-file))


(provide 'ext-merging)
