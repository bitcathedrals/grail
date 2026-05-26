;;
;; grail-diff.el
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

(defun grail-diff-check-diff-args (local-file upstream-file &optional ancestor-file)
  (if (and (grail-diff-is-arg-ok local-file)
           (grail-diff-is-arg-ok upstream-file))
    (if ancestor-file
      (grail-diff-is-arg-ok ancestor-file)
      t)
    nil))

;;
;; handle windows save/restore labeling and management
;;

(defvar grail-diff-buffer-from nil "grail-diff store the buffer from")

(defun grail-diff-save-buffer ()
  (setq grail-diff-buffer-from (window-buffer (selected-window))))

(defun grail-diff-restore-buffer ()
  (switch-to-buffer grail-diff-buffer-from))

(defun grail-diff-delete-frame ()
  "ediff-close-delete-frame

  delete the frame"
  (interactive)
  (when (> (length (visible-frame-list)) 1)
    (delete-frame (selected-frame)) ))

(defun grail-diff-quit-message ()
  (message "grail-diff-quit-message run"))

(defun grail-diff-apply-visual (buffer label)
  (when (bufferp buffer)
    (with-current-buffer buffer
      (when (not (boundp 'grail-diff-done))
        (let
          ((first-elm (car mode-line-format))
           (rest-elm  (cdr mode-line-format)))

          (setq mode-line-format (append first-elm (list label) rest-elm))
          (set (make-local-variable 'grail-diff-done) t)) ))))

(defun grail-diff-visual-changes ()
  "grail-configure-ediff-change-window-names

   re-label the ediff windows
  "
  (grail-diff-apply-visual ediff-buffer-A "{local} ")
  (grail-diff-apply-visual ediff-buffer-B "{upstream} ")
  (when (and (boundp 'ediff-buffer-C)
             (bufferp ediff-buffer-C))
    (grail-diff-apply-visual ediff-buffer-C "{merge} "))
  (force-mode-line-update))

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

(defun grail-diff-open-session ()
  (grail-diff-save-buffer)
  (grail-diff-save-window-state))

(defun grail-diff-close-session ()
  (interactive)
  (message "grail-diff: closing ediff session.")

  (ediff-quit nil)

  (grail-diff-delete-frame)
  (grail-diff-restore-window-state)
  (grail-diff-restore-buffer))

(defun grail-diff-elisp (file-local file-upstream)
  (if (grail-diff-check-diff-args file-local file-upstream)
    (progn
      (grail-diff-open-session)
      (ediff-buffers
        (find-file-noselect file-local)
        (find-file-noselect file-upstream)))
    (message "grail-diff-elisp: arguments 2way %s %s not valid"
             file-local file-upstream)) )

(defun grail-diff-revision (file)
    (interactive "fFile to diff")
    (ediff-revision file '(grail-diff-open-session)))

(defun grail-diff-ancestor-elisp (file-local file-upstream file-ancestor)
  (grail-diff-3way-setup)

  (if (grail-diff-check-diff-args file-local file-upstream file-ancestor)
    (progn
      (grail-diff-open-session)

      (ediff-buffers3
        (find-file-noselect file-local)
        (find-file-noselect file-upstream)
        (find-file-noselect file-ancestor)))
    (message "grail-diff-elisp: arguments 3way %s %s %s not valid"
             file-local file-upstream file-ancestor)) )

;;
;; merging
;;

(defun grail-diff-merge-elisp (file-local file-upstream)
  (grail-diff-open-session)

  (ediff-merge-files
    (find-file-noselect file-upstream)
    (find-file-noselect file-local) ))

(defun grail-diff-merge-ancestor-elisp (file-local file-upstream file-ancestor)
  (grail-diff-open-session)

  (ediff-merge-buffers-with-ancestor
    (find-file-noselect file-local)
    (find-file-noselect file-upstream)
    (find-file-noselect file-ancestor) ))

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

(defun grail-diff-keys ()
  (local-set-key (kbd "C-c !") 'grail-diff-close-session))

(defun grail-diff-configure ()
  "grail-diff-configure
   configure the grail extensions and customization of the ediff tool"
  (interactive)

  (add-hook 'ediff-after-setup-windows-hook 'grail-diff-visual-changes)
  (add-hook 'ediff-after-setup-windows-hook 'grail-diff-keys)

  (setq-default ediff-split-window-function 'split-window-horizontally)
  (setq-default ediff-merge-split-window-function 'split-window-vertically)
  (setq-default ediff-window-setup-function 'ediff-setup-windows-plain)

  (setq-default ediff-keep-variants nil)
  (setq-default ediff-auto-refine 'on))

(provide 'grail-diff)
