;; -*-no-byte-compile: t; -*-

;;----------------------------------------------------------------------
;; accumulate buffer status
;;
;; when multiple status messages are needed accumulate the status messes
;; and display the list at the end.
;;----------------------------------------------------------------------
(defvar-local buffer-status-list nil)

(defun buffer-display-status ()
  (interactive)

  (when buffer-status-list
    (message "%s" (string-join buffer-status-list ", ")) ))

(defun buffer-status-add ( message )
  (add-to-list 'buffer-status-list message))

(provide 'buffer-status)
