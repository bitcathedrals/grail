;;
;; git configuration
;;

;;
;; magit
;;

;; magit breaks frequently so put it into a general git profile
(require 'magit)

;; refresh after edit
(add-hook 'after-save-hook 'magit-after-save-refresh-status t)

(setq git-commit-style-convention-checks
      (remove 'non-empty-second-line git-commit-style-convention-checks))

(custom-key-group "magit git" "v" t
  ("v" . magit-status)
  ("l" . magit-log)

  ("e" . magit-ediff-dwim)

  ("a" . magit-stage)
  ("u" . magit-unstage)
  ("c" . magit-commit)
  ("&" . magit-commit-squash)
  ("x" . magit-commit-amend)

  ("+" . magit-ediff-show-staged)
  ("*" . magit-ediff-show-unstaged)
  ("s" . magit-ediff-show-stash)

  ("r" . magit-ediff-resolve-all)
  ("p" . magit-push))

(provide 'profile/git)
