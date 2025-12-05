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
  ;; ediff has a function to strip the modeline if the first member of modeline
  ;; is not one of the prefixes. if this becomes a problem insert our label
  ;; after.

  (when ediff-buffer-A
    (with-current-buffer ediff-buffer-A
      (when (not (boundp 'grail-diff-A-modeline))
        (setq mode-line-format (cons "local:" mode-line-format))
        (set (make-local-variable 'grail-diff-A-modeline) t)
        (force-mode-line-update)) ))

  (when ediff-buffer-B
    (with-current-buffer ediff-buffer-B
      (when (not (boundp 'grail-diff-B-modeline))
        (setq
          ediff-diff-status (append "upstream:" ediff-diff-status))
        (set (make-local-variable 'grail-diff-B-modeline) t))
      (force-mode-line-update)) )

  (when ediff-buffer-C
    (with-current-buffer ediff-buffer-C
      (when (not (boundp 'grail-diff-C-modeline))
        (setq
          ediff-diff-status (append "merge:" ediff-diff-status))
        (set (make-local-variable 'grail-diff-C-modeline) t))
      (force-mode-line-update)) ) )

(defun grail-configure-ediff ()
  "configure-ediff

   configure the ediff tool"
  (interactive)

  (setq-default ediff-split-window-function 'split-window-horizontally)
  (setq-default ediff-merge-split-window-function 'split-window-vertically)
  (setq-default ediff-window-setup-function 'ediff-setup-windows-plain)

  (setq-default ediff-keep-variants nil)

  (add-hook 'ediff-quit-hook 'ediff-close-buffer-and-frame)

  (add-hook 'ediff-after-setup-windows-hook 'grail-configure-ediff-change-window-names) )

(provide 'grail-diff)
