;;
;; grail-ediff.el
;;
;; customization of ediff to make it more functional, intuitive and robust.
;;

;;
;; handle windows labeling and management
;;

(defun grail-diff-close-buffer-and-frame ()
  "ediff-close-buffer-and-frame

  close the frame hosting the ediff buffers"
  (interactive)
  (kill-buffer (current-buffer))

  (when (> (length (frame-list)) 1)
    (delete-frame (selected-frame)) ))

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

;;
;; turn source buffers into readonly
;;

(defun grail-diff-readonly ()
  (with-current-buffer ediff-buffer-A
    (setq buffer-read-only t))

  (with-current-buffer ediff-buffer-B
    (setq buffer-read-only t)) )

(defun grail-diff-readonly-ancestor ()
  (with-current-buffer ediff-buffer-C
    (setq buffer-read-only t)))

;;
;; handle 3way merge/diff which is vertical instead of horizontally to better manage screen real estate.
;;

(defun grail-diff-3way-frame-before ()
  (setq-default ediff-split-window-function 'split-window-vertically))

(defun grail-diff-3way-frame-after ()
  (setq-default ediff-split-window-function 'split-window-horizontally))

(defun grail-diff-3way-teardown ()
  (remove-hook 'ediff-before-setup-hook 'grail-diff-3way-frame-before)
  (remove-hook 'ediff-quit-hook 'grail-diff-3way-frame-after) )

(defun grail-diff-3way-setup ()
  (add-hook 'ediff-before-setup-hook 'grail-diff-3way-frame-before)
  (add-hook 'ediff-after-quit-hook-internal 'grail-diff-3way-frame-after)
  (add-hook 'ediff-after-quit-hook-internal 'grail-diff-3way-teardown) )

(defun grail-diff-elisp (file-local file-upstream)
  (ediff-buffers
    (find-file-noselect file-local)
    (find-file-noselect file-upstream)) )

(defun grail-diff-ancestor-elisp (file-local file-upstream file-ancestor)
  (grail-diff-3way-setup)

  (ediff-buffers3
    (find-file-noselect file-local)
    (find-file-noselect file-upstream)
    (find-file-noselect file-ancestor)
    '(grail-diff-readonly-ancestor)) )

(defun grail-diff-get-merge-buffer (local-file upstream-file)
  (get-buffer-create (grail-diff-merge-file-name local-file upstream-file)) )

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
      (format-time-string "%H:%M")
      "." extension)))

(defun grail-diff-merge-elisp (file-local file-upstream)
  (ediff-merge-buffers
    (find-file-noselect file-local)
    (find-file-noselect file-upstream)
    '()
    'ediff-merge-buffers
    (grail-diff-merge-file-name file-local file-upstream)) )

(defun grail-diff-merge-ancestor-elisp (file-local file-upstream file-ancestor)
  (ediff-merge-buffers-with-ancestor
    (find-file-noselect file-local)
    (find-file-noselect file-upstream)
    (find-file-noselect file-ancestor)
    'ediff-merge-buffers-with-ancestor
    (grail-diff-merge-file-name file-local file-upstream)) )

;;
;; This will not work, need to make a helm buffer of found merges.
;;

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

  (add-hook 'ediff-prepare-buffers-hook 'grail-diff-readonly)
  (add-hook 'ediff-after-setup-windows-hook 'grail-diff-relabel-window-names)

  (add-hook 'ediff-before-setup-hook 'grail-diff-save-window-state)
  (add-hook 'ediff-after-quit-hook-internal 'grail-diff-restore-window-state)

  (setq-default ediff-split-window-function 'split-window-horizontally)
  (setq-default ediff-merge-split-window-function 'split-window-vertically)
  (setq-default ediff-window-setup-function 'ediff-setup-windows-multiframe)

  (setq-default ediff-keep-variants nil)
  (setq-default ediff-auto-refine 'on)

  (add-hook 'ediff-quit-hook 'grail-diff-close-buffer-and-frame) )

(provide 'grail-diff)
