;; -*-no-byte-compile: t; -*-

(defun goto-scheme ()
  (interactive)
  (dired (concat (getenv "HOME") "/code/algorithms/src/scheme/sicp/")) )

(defun goto-cl ()
  (interactive)
  (dired (concat (getenv "HOME") "/code/algorithms/src/cl/")) )

(defun goto-emacs ()
  (interactive)
  (dired (concat (getenv "HOME") "/.emacs.grail/")) )
