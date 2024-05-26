(require 'gnus)
(require 'sensitive)

(setq
  gnus-select-method '(nnnil "")
  gnus-parameters '((".*" (display . all))) )

(add-to-list 'gnus-secondary-select-methods
  '(nnmaildir "proton" (directory "~/mail/")))
