;;----------------------------------------------------------------------
;; user-display.el
;;----------------------------------------------------------------------

(defconst codermattie-bg-color "grey5")

(defun display-faces-terminal ()
  (set-face-background 'font-lock-type-face codermattie-bg-color)
  (set-face-foreground 'font-lock-type-face "green4")

  (set-face-background 'font-lock-function-name-face codermattie-bg-color)
  (set-face-foreground 'font-lock-function-name-face "aquamarine4")

  (set-face-background 'font-lock-variable-name-face codermattie-bg-color)
  (set-face-foreground 'font-lock-variable-name-face "aquamarine3")

  (set-face-background 'help-key-binding codermattie-bg-color)
  (set-face-foreground 'help-key-binding "green4"))

(defun display-faces-general ()
  (set-face-background 'default codermattie-bg-color)
  (set-face-foreground 'default "grey55"))

  ;; comments are set off-tempature to distingiush them better.
  ;; orange was chosen as a red that wasn't harsh.
  (set-face-background 'font-lock-comment-face codermattie-bg-color)
  (set-face-foreground 'font-lock-comment-face "orange3")

  ;; grammar is the lightest shade of blue
  (set-face-background 'font-lock-keyword-face codermattie-bg-color)
  (set-face-foreground 'font-lock-keyword-face "DeepSkyBlue4")

  (set-face-background 'font-lock-builtin-face codermattie-bg-color)
  (set-face-foreground 'font-lock-builtin-face "DeepSkyBlue4")

  ;; this should be for any form of literal value in code medium contrast.
  (set-face-background 'font-lock-string-face codermattie-bg-color)
  (set-face-foreground 'font-lock-string-face "grey50")

  (set-face-background 'font-lock-constant-face codermattie-bg-color)
  (set-face-foreground 'font-lock-constant-face "grey50")

  (set-face-background 'font-lock-constant-face codermattie-bg-color)
  (set-face-foreground 'font-lock-constant-face "grey50")

  (set-face-background 'font-lock-warning-face codermattie-bg-color)
  ;;(set-face-attribute  'font-lock-warning-face :underline "red")
  (set-face-foreground 'font-lock-warning-face "grey70")

(display-faces-terminal)
(display-faces-general)

(provide 'user-display)
