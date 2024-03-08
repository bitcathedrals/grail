;; -*-no-byte-compile: t; -*-

;;----------------------------------------------------------------------
;; activate buffer status. done as a profile so we can do this last.
;;----------------------------------------------------------------------
(require 'buffer-status)

(add-hook 'after-change-major-mode-hook 'buffer-display-status t)

(provide 'profile/activate-buffer-status)
