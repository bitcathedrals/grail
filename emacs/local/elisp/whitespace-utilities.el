;; -*-no-byte-compile: t; -*-

;;----------------------------------------------------------------------
;; whitespace handling
;;----------------------------------------------------------------------
(require 'whitespace)
(require 'buffer-status)

(defun buffer-modifiable-p (buffer)
  (with-current-buffer buffer
    (not (or (and (local-variable-p 'view-read-only) view-read-only)
             (and (local-variable-p 'buffer-read-only) buffer-read-only))) ))

(defun warn-if-tabs-in-buffer ()
  (interactive)

  (save-excursion
    (beginning-of-buffer)
    (when (search-forward-regexp "	" nil t)
      (buffer-status-add "!WARNING! TAB chacters in buffer!"))))

(defun scrub-tabs ()
  (interactive)

  (save-excursion
    (let
      ((end-point (progn
                    (end-of-buffer)
                    (point)))
        (begin-point
          (progn
            (beginning-of-buffer)
            (point)))
        (result 0))

      (setq result (replace-regexp "	" " " nil begin-point end-point))

      (when (and result (> result 0))
        (message "WARNING: %s tab characters found!" result)) )) )

;;----------------------------------------------------------------------
;;                   whitespace detection and correction
;;----------------------------------------------------------------------

(defun update-whitespace-mappings ( type char map-to )
  (let
    ((new-mapping  nil)
     (replaced nil))

    (mapc (lambda ( mapping )
            (unless (eq (car mapping) type)
              (setq new-mapping (cons mapping new-mapping)) ))
      whitespace-display-mappings)

    (setq whitespace-display-mappings
      (cons (list type char (make-vector 1 map-to))
            (reverse new-mapping))) ))

;; empty does not work
(setq whitespace-style '(face trailing tabs tab-mark))

(update-whitespace-mappings 'tab-mark ?	 ?ɤ)
(update-whitespace-mappings 'space-mark (elt " " 0) ?ɤ)
(update-whitespace-mappings 'newline-mark (elt "\n" 0) ?ɤ)

(add-hook 'find-file-hook 'warn-if-tabs-in-buffer t)

(provide 'whitespace-utilities)
