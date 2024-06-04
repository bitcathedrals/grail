;; -*-no-byte-compile: t; -*-

(require 'buffer-status)

(setq after-change-major-mode-hook
  (append after-change-major-mode-hook '(buffer-display-status)))

(provide 'profile/activate-buffer-status)
