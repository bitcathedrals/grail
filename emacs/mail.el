(require 'gnus)

;; it tries to pull bbdb-site which I will fix later
;; (require 'bbdb)

(require 'sensitive)

(setq
  gnus-select-method '(nntp "news.eweka.nl")
  gnus-parameters '((".*" (display . all))) )

(add-to-list
  'gnus-secondary-select-methods
  '(nnmaildir "proton" (directory "~/mail/")))

(setq
  bbdb-complete-name-allow-cycling t
  bbdb-use-pop-up nil)

