;;----------------------------------------------------------------------
;; gnupg crypto support
;;----------------------------------------------------------------------
(require 'custom-key)

(require 'epg)
(require 'epa-file)

;; Enable loopback so that pinentry will pop up in emacs
(setq
  epg-user-id "codermattie@runbox.com")

(custom-key-group "crypto" "z" t
  ("k" . epa-list-keys)
  ("e" . epa-encrypt-region)
  ("s" . epa-sign-region)
  ("d" . epa-decrypt-region)
  ("v" . epa-verify-region)
  ("f" . dired))

(provide 'crypto)
