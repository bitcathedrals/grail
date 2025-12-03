(defun ediff-close-buffer-and-frame ()
  "ediff-close-buffer-and-frame

  close the frame hosting the ediff buffers"
  (interactive)
  (let*
    ((buf (current-buffer))
    (frame (selected-frame)))

    (kill-buffer buf)
    (delete-frame frame)) )

(defun grail-configure-ediff ()
  "configure-ediff

   configure the ediff tool"
  (interactive)

  (setq-default ediff-split-window-function 'split-window-horizontally)
  (setq-default ediff-merge-split-window-function 'split-window-vertically)

  (setq-default ediff-window-setup-function 'ediff-setup-windows-plain)
  (setq-default ediff-keep-variants nil)

  (add-hook 'ediff-quit-hook 'ediff-close-buffer-and-frame) )
