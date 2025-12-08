(defun grail-diff-close-buffer-and-frame ()
  "ediff-close-buffer-and-frame

  close the frame hosting the ediff buffers"
  (interactive)
  (kill-buffer (current-buffer))

  (when (> (length (frame-list)) 1)
    (delete-frame (current-frame)) ))

(defun grail-diff-insert-label-into-modeline (buffer guard label)
  (when (bufferp buffer)
    (with-current-buffer buffer
      (when (not (boundp guard))
        (let
          ((first-elm (car mode-line-format))
           (rest-elm  (cdr mode-line-format)))

          (setq mode-line-format (append first-elm (list label) rest-elm))
          (force-mode-line-update)

          (set (make-local-variable guard) t)) ))))

(defun grail-diff-relabel-window-names ()
  "grail-configure-ediff-change-window-names

   re-label the ediff windows
  "
  (grail-diff-insert-label-into-modeline ediff-buffer-A 'grail-diff-A-guard "{local}")
  (grail-diff-insert-label-into-modeline ediff-buffer-B 'grail-diff-B-guard "{upstream}")
  (grail-diff-insert-label-into-modeline ediff-buffer-C 'grail-diff-C-guard "{merge}") )

;; I haven't figured out what or how the ancestor buffer works
;;  (grail-diff-insert-label-into-modeline ediff-buffer-ancestor 'grail-diff-ancestor-guard "{ancestor}") )

(defun grail-diff-merge-file-name (local-file upstream-file)
  (let*
    ((local-base (file-name-nondirectory local-file))
     (upstream-base (file-name-nondirectory upstream-file))

     (extension (file-name-extension local-base))

     (local-stripped (file-name-sans-extension local-base))
     (upstream-stripped (file-name-sans-extension upstream-base)) )

    (concat
      "ediff-merge-"
      local-stripped "-"
      upstream-stripped "-"
      "[" extension "]-"
      (format-time-string "%H:%M"))) )

(defun grail-diff-get-merge-buffer (local-file upstream-file)
  (get-buffer-create (grail-diff-merge-file-name local-file upstream-file)) )

(defun grail-diff-elisp (file-local file-upstream)
  (ediff-buffers
    (find-file-noselect file-local)
    (find-file-noselect file-upstream)) )

(defun grail-diff-windows-before-ancestor ()
  (setq-default ediff-split-window-function 'split-window-vertically))

(defun grail-diff-windows-after-ancestor ()
  (setq-default ediff-split-window-function 'split-window-horizontally))

(defun grail-diff-ancestor-elisp (file-local file-upstream file-ancestor)
  (grail-diff-windows-before-ancestor)

  (ediff-buffers3
    (find-file-noselect file-local)
    (find-file-noselect file-upstream)
    (find-file-noselect file-ancestor))

  (grail-diff-windows-after-ancestor) )

(defun grail-diff-merge-elisp (file-local file-upstream file-ancestor)
  (ediff-merge-buffers-with-ancestor
    (find-file-noselect file-local)
    (find-file-noselect file-upstream)
    (grail-diff-get-merge-file-name file-local)) )

(defun grail-diff-merge-ancestor-elisp (file-local file-upstream file-ancestor)
  (ediff-files)

  (ediff-merge-buffers-with-ancestor
    (find-file-noselect file-local)
    (find-file-noselect file-upstream)) )

(defun grail-resume-merge (file-local file-upstream)
  ;; make a list of merges to resume and make a helm buffer out of it
  (interactive "fresume-local:\nfresume-upstream:") )

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

(defun grail-diff-configure ()
  "grail-diff-configure

   configure the grail extensions and customization of the ediff tool"
  (interactive)

  (add-hook 'ediff-before-setup-hook 'grail-diff-save-window-state)

  (add-hook 'ediff-after-quit-hook-internal 'grail-diff-restore-window-state)

  (setq-default ediff-split-window-function 'split-window-horizontally)
  (setq-default ediff-merge-split-window-function 'split-window-vertically)
  (setq-default ediff-window-setup-function 'ediff-setup-windows-plain)

  (setq-default ediff-keep-variants nil)
  (setq-default ediff-auto-refine t)

  (add-hook 'ediff-quit-hook 'grail-diff-close-buffer-and-frame)
  (add-hook 'ediff-after-setup-windows-hook 'grail-diff-relabel-window-names))

(provide 'grail-diff)
