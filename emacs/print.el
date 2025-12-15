(require 'ps-print)

(defun pdf-print-buffer-with-faces (&optional filename)
  (interactive (list (if current-prefix-arg
                       (ps-print-preprint 4)
                       (concat (file-name-sans-extension (buffer-file-name))
                         ".ps"))))

  (ps-print-region-with-faces (point-min) (point-max) filename)

  (shell-command (concat "ps2pdf " filename))

  (delete-file filename)

  (message "Deleted %s" filename)
  (message "Wrote %s" (concat (file-name-sans-extension filename) ".pdf")) )
