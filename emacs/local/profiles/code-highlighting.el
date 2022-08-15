;;----------------------------------------------------------------------
;; code highlighting tools
;;----------------------------------------------------------------------

;; mic-paren fancy paren/delimited highlighting. It is particularly
;;           valuable for reverse highlighting regions.

(require 'mic-paren)

;;----------------------------------------------------------------------
;; paren mode configuration - most important mode of them all
;;----------------------------------------------------------------------
(setq
  paren-showing t
  show-paren-style 'parenthesis
  show-paren-delay 1
  paren-sexp-mode 'match)

(paren-activate)

(provide 'profile/code-highlighting)
