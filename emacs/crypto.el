;; -*-no-byte-compile: t; -*-

(require 'custom-key)
(require 'sensitive)

(require 'epg)
(require 'epa-file)

(custom-key-group "crypto" "z" t
  ("k" . epa-list-keys)
  ("e" . epa-encrypt-region)
  ("s" . epa-sign-region)
  ("d" . epa-decrypt-region)
  ("v" . epa-verify-region)
  ("f" . dired))

(provide 'crypto)
