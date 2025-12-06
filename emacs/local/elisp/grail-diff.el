(defun ediff-close-buffer-and-frame ()
  "ediff-close-buffer-and-frame

  close the frame hosting the ediff buffers"
  (interactive)
  (let*
    ((buf (current-buffer))
    (frame (selected-frame)))

    (kill-buffer buf)
    (delete-frame frame)) )

(defun insert-ediff-label-into-modeline (label)
  (let
    ((first-elm (car mode-line-format))
     (rest-elm  (cdr mode-line-format)))

    (setq mode-line-format (append first-elm (list label) rest-elm))
    (print mode-line-format))
  (force-mode-line-update))

(defun grail-configure-ediff-change-window-names ()
  "grail-configure-ediff-change-window-names

   re-label the ediff windows
  "
  (when ediff-buffer-A
    (with-current-buffer ediff-buffer-A
      (when (not (boundp 'grail-diff-A-modeline))
        (insert-ediff-label-into-modeline "{local}") )) )

  (when ediff-buffer-B
    (with-current-buffer ediff-buffer-B
      (when (not (boundp 'grail-diff-B-modeline))
        (insert-ediff-label-into-modeline "{upstream}") )) )

  (when ediff-buffer-C
    (with-current-buffer ediff-buffer-C
      (when (not (boundp 'grail-diff-C-modeline))
        (insert-ediff-label-into-modeline "{merge}") )) ) )

;;
;; nifty post that showed me a lot of things like save/restore
;; http://yummymelon.com/devnull/using-ediff-in-2023.html
;;

;; taken from ediff-2023, use imoji so the configuration doesn't get stepped on.

(defun grail-diff-save-window-state ()
  (window-configuration-to-register ?🧊))

(defun grail-diff-restore-window-state ()
  "Restore window configuration from register 🧊."
  (jump-to-register ?🧊))

(defun grail-configure-ediff ()
  "configure-ediff

   configure the ediff tool"
  (interactive)

  (add-hook 'ediff-before-setup-hook 'grail-diff-save-window-state)
  (add-hook 'ediff-after-quit-hook-internal 'grail-diff-restore-window-state)

  (setq-default ediff-split-window-function 'split-window-horizontally)
  (setq-default ediff-merge-split-window-function 'split-window-vertically)
  (setq-default ediff-window-setup-function 'ediff-setup-windows-plain)

  (setq-default ediff-keep-variants nil)

  (add-hook 'ediff-quit-hook 'ediff-close-buffer-and-frame)
  (add-hook 'ediff-after-setup-windows-hook 'grail-configure-ediff-change-window-names) )

(provide 'grail-diff)
