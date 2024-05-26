(require 'gnus)
(require 'bbdb)

(require 'sensitive)

(setq
  gnus-select-method '(nnnil "")
  gnus-parameters '((".*" (display . all))) )

(add-to-list 'gnus-secondary-select-methods
  '(nnmaildir "proton" (directory "~/mail/")))

(setq
  bbdb-complete-name-allow-cycling t
  bbdb-use-pop-up nil)

