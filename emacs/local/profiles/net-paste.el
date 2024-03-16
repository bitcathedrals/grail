;; -*-no-byte-compile: t; -*-

(require 'dpaste)

(defconst npaste-default-name user-login-name)

(defun npaste-default-title ()
  (concat npaste-default-name "/"
          (format-time-string "%a(%H:%M:%S)" (current-time))) )

;; dpaste

(defun npaste-title ()
  (let
    ((paste-title (read-string "title? ")))

    (if (< (length paste-title) 1)
      (npaste-default-title)
      (concat (npaste-default-title) "-" paste-title)) ))

(defun npaste-region ()
  (interactive)
  (dpaste-region (mark) (point) (npaste-dpaste-title)))

(defun npaste-buffer ()
  (interactive)
  (save-exursion
    (mark-whole-buffer)
    (dpaste-buffer (npaste-dpaste-title)) ))

(provide 'profile/paste)
