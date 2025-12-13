;;
;; grail-ediff.el
;;
;; customization of ediff to make it more functional, intuitive and robust.
;;

;;
;; error checking
;;

(defun grail-diff-is-arg-ok (argument)
  (ignore-errors
    (if (and (stringp argument)
             (not (string-equal argument "")))
      t
      nil)) )

(defun grail-diff-check-diff-args (local-file upstream-file)
  (if (and (grail-diff-is-arg-ok local-file)
           (grail-diff-is-arg-ok upstream-file))
    t
    nil))

(defun grail-diff-check-ancestor-arg (ancestor-file)
  (grail-diff-is-arg-ok ancestor-file))

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
  (grail-diff-insert-label-into-modeline ediff-buffer-A 'grail-diff-A-guard "{upstream}")
  (grail-diff-insert-label-into-modeline ediff-buffer-B 'grail-diff-B-guard "{local}")
  (grail-diff-insert-label-into-modeline ediff-buffer-C 'grail-diff-C-guard "{merge}") )

;;
;; line-number-mode
;;

(defun grail-line-numbers ()
  (with-current-buffer ediff-buffer-A
    (line-number-mode))

  (with-current-buffer ediff-buffer-B
    (line-number-mode)) )

(defun grail-line-numbers-C ()
  (with-current-buffer ediff-buffer-C
    (line-number-mode)) )

;;
;; turn source buffers into readonly
;;

(defvar grail-diff-buffer-toggle-list nil)

(defun grail-diff-clear-toggle-list ()
  (setq grail-diff-buffer-toggle-list nil))

(defun grail-diff-readonly ()
  (with-current-buffer ediff-buffer-A
    (setq buffer-read-only t)
    (setq grail-diff-buffer-toggle-list
      (append (list ediff-buffer-A) grail-diff-buffer-toggle-list)))

  (with-current-buffer ediff-buffer-B
    (setq buffer-read-only t)
    (setq grail-diff-buffer-toggle-list
      (append (list ediff-buffer-B) grail-diff-buffer-toggle-list))) )

(defun grail-diff-readonly-C ()
  (with-current-buffer ediff-buffer-C
    (setq grail-diff-buffer-toggle-list
      (append (list ediff-buffer-C) grail-diff-buffer-toggle-list))) )

;;
;; turn them back to read/write
;;

(defun grail-diff-toggle-rw (buffer)
  (ignore-errors
    (with-current-buffer buffer
      (setq buffer-read-only nil))) )

(defun grail-diff-toggle-rw-all ()
  (mapcar 'grail-diff-toggle-rw grail-diff-buffer-toggle-list))

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

;;
;; NOTE: the files are swapped around for ediff because for some bizarre
;;       reason ediff puts the upstream in A, and the local in B by default

(defun grail-diff-elisp (file-local file-upstream)
  (if (grail-diff-check-diff-args file-local file-upstream)
    (ediff-buffers
      (find-file-noselect file-local)
      (find-file-noselect file-upstream))
    (message "grail-diff-elisp: arguments %s %s not valid" file-local file-upstream)) )

(defun grail-diff-ancestor-elisp (file-local file-upstream file-ancestor)
  (grail-diff-3way-setup)

  (ediff-buffers3
    (find-file-noselect file-local)
    (find-file-noselect file-upstream)
    (find-file-noselect file-ancestor)
    '(grail-line-numbers-C grail-diff-readonly-C)) )

;;
;; merging
;;

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
    (grail-line-numbers-C)
    'ediff-merge-buffers
    (grail-diff-merge-file-name file-local file-upstream)) )

(defun grail-diff-merge-ancestor-elisp (file-local file-upstream file-ancestor)
  (ediff-merge-buffers-with-ancestor
    (find-file-noselect file-local)
    (find-file-noselect file-upstream)
    (find-file-noselect file-ancestor)
    '(grail-line-numbers-C)
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

  ;; this goes in reverse to the natural order since add-hook adds to the
  ;; front
  (add-hook 'ediff-after-setup-windows-hook 'grail-diff-readonly)
  (add-hook 'ediff-after-setup-windows-hook 'grail-line-numbers)
  (add-hook 'ediff-after-setup-windows-hook 'grail-diff-relabel-window-names)

  (add-hook 'ediff-before-setup-hook 'grail-diff-save-window-state)
  (add-hook 'ediff-before-setup-hook 'grail-diff-clear-toggle-list)

  (advice-add 'ediff-buffers :after 'grail-diff-readonly)
  (advice-add 'ediff-buffers3 :after 'grail-diff-readonly)

  (advice-add 'ediff-merge-buffers :after 'grail-diff-readonly)
  (advice-add 'ediff-merge-buffers-with-ancestor :after 'grail-diff-readonly-C)

  (advice-add 'ediff-merge-buffers :after 'grail-diff-readonly)
  (advice-add 'ediff-merge-buffers-with-ancestor :after 'grail-diff-readonly-C)

  ;; the quit-internal runs after every file, make sure we don't run
  ;; this stuff until the whole set has run
  (add-hook 'ediff-quit-hook 'grail-diff-toggle-rw-all)
  (add-hook 'ediff-quit-hook 'grail-diff-restore-window-state)
  (add-hook 'ediff-quit-hook 'grail-diff-close-buffer-and-frame)

  (setq-default ediff-split-window-function 'split-window-horizontally)
  (setq-default ediff-merge-split-window-function 'split-window-vertically)
  (setq-default ediff-window-setup-function 'ediff-setup-windows-plain)

  (setq-default ediff-keep-variants nil)
  (setq-default ediff-auto-refine 'on) )

(provide 'grail-diff)
