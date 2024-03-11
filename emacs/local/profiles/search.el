(require 'custom-key)
(require 'visual-regexp-steroids)

(custom-key-group "search" "s" t
      ("g" . grep)
      ("r" . rgrep)
      ("o" . occur)
      ("f" . vr/isearch-forward)
      ("b" . vr/backward)
       "t" . vr/query-replace)

(provide 'profile/search)


