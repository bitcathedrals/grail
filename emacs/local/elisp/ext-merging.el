;; -*-no-byte-compile: t; -*-

(defun ext-merge (local-file upstream-file)
  (interactive "fLocal :\nfUpstream: ")
  (ediff-merge-files local-file upstream-file))

(defun ext-merge3 (local-file upstream-file ancestor-file)
  (interactive "fLocal :\nfUpstream: \nAncestor: ")
  (ediff-merge-files-with-ancestor local-file upstream-file ancestor-file))

(defun ext-diff (local-file upstream-file)
  "ext-diff BASE-FILE CHANGED-FILE

   diff CHANGED-FILE against changes in CHANGE-FILE
  "
  (interactive "fLocal: \nfUpstream: ")
  (ediff-files local-file upstream-file))

(defun ext-diff3 (local-file upstream-file ancestor-file)
  "ext-diff LEFT-FILE RIGHT-FILE ANCESTOR-FILE

   diff LEFT-FILE and RIGHT-FILE against ANCESTOR-FILE
  "
  (interactive "fLocal: \nfUpstream: \nAncestor: ")
  (ediff-files3 local-file upstream-file ancestor-file))


(provide 'ext-merging)
