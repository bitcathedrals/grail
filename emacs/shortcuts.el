(defun goto-scheme ()
  (interactive)
  (dired (concat (getenv "HOME") "/code/algorithms/src/scheme/sicp/")) )

(defun goto-emacs ()
  (interactive)
  (dired (concat (getenv "HOME") "/.emacs.grail/")) )
