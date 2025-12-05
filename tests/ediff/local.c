(defun ediff-close-buffer-and-frame ()
  "ediff-close-buffer-and-frame

  close the frame hosting the ediff buffers"
  (interactive)
  (let*
    ((buf (current-buffer))
    (frame (selected-frame)))

    (kill-buffer buf)
    (delete-frame frame)) )

(defun grail-configure-ediff-change-window-names ()
  "grail-configure-ediff-change-window-names

   re-label the ediff windows
  "
  (when ediff-buffer-A
    (with-current-buffer ediff-buffer-A
      (setq mode-line-format
        (list "local:" 'ediff-diff-status mode-line-format))
      (force-mode-line-update)) )

  (when ediff-buffer-B
    (with-current-buffer ediff-buffer-B
      (setq mode-line-format
        (list "upstream: " 'ediff-diff-status mode-line-format))
      (force-mode-line-update)) )

      (force-mode-line-update)) ))

(defun grail-configure-ediff ()
  "configure-ediff

   configure the ediff tool"
  (interactive)

  (setq-default ediff-keep-variants nil)

  (add-hook 'ediff-quit-hook 'ediff-close-buffer-and-frame)

  (add-hook 'ediff-after-setup-windows-hook 'grail-configure-ediff-change-window-names) )

(provide 'grail-diff)
