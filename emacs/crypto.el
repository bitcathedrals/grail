;;----------------------------------------------------------------------
;; gnupg crypto support
;;----------------------------------------------------------------------

;; use pinentry-mac from homebrew on MacOS

(require 'epg)
(require 'epa-file)

;; Enable loopback so that pinentry will pop up in emacs
(setq
  epg-user-id "codermattie@runbox.com")

(provide 'crypto)
