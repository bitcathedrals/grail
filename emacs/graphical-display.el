;; -*-no-byte-compile: t; -*-
;;----------------------------------------------------------------------
;; graphical-display
;;----------------------------------------------------------------------
(require 'subr-x)

(defvar grail-frame-width 120 "default width of the frame in characters")
(defvar grail-frame-height 80 "default height of the frame in characters")
(defvar grail-font-size 18 "default font size")
(defvar grail-transparency 100 "default transparency is opaque")

;;
;; GUI detection
;;

(defun is-frame-gui ( &optional this-frame )
  "is-current-frame-x FRAME

   Return t if FRAME or (selected-frame) is a GUI frame, nil
   otherwise.
  "
  (let
    ((frame-type (framep this-frame)))

    (when (and frame-type
            (or
              (equal 'x frame-type)
              (equal 'w32 frame-type)
              (equal 'ns frame-type)))
      t) ))

;;
;; font stuff
;;

(defun grail-select-best-font-family ( frame )
  (catch 'best-font
    (dolist (canidate grail-font-family)
      (when (member canidate (font-family-list frame))
        (throw 'best-font canidate)) )

    (car (font-family-list frame)) ))

(defun grail-build-font ( frame )
  (concat (grail-select-best-font-family frame) "-" (int-to-string grail-font-size)) )

;;
;; frame parameters
;;

(defun grail-graphical-frame-configuration ( frame )
  (list
    '(underline . nil)
    '(inverse-video . nil)
    '(box . nil)
    '(strike-through . nil)
    '(overline . nil)
    '(mouse-color . "red")
    '(cursor-color . "yellow")
    `(width . ,grail-frame-width)
    `(height . ,grail-frame-height)) )

(defconst personal-bg-color "grey5")

(defun display-faces-general ()

  (set-face-background 'font-lock-type-face personal-bg-color)
  (set-face-foreground 'font-lock-type-face "green4")

  (set-face-background 'font-lock-function-name-face personal-bg-color)
  (set-face-foreground 'font-lock-function-name-face "aquamarine4")

  (set-face-background 'font-lock-variable-name-face personal-bg-color)
  (set-face-foreground 'font-lock-variable-name-face "aquamarine3")

  ;; comments are set off-tempature to distingiush them better.
  ;; orange was chosen as a red that wasn't harsh.
  (set-face-background 'font-lock-comment-face personal-bg-color)
  (set-face-foreground 'font-lock-comment-face "orange3")

  ;; grammar is the lightest shade of blue
  (set-face-background 'font-lock-keyword-face personal-bg-color)
  (set-face-foreground 'font-lock-keyword-face "DeepSkyBlue4")

  (set-face-background 'font-lock-builtin-face personal-bg-color)
  (set-face-foreground 'font-lock-builtin-face "DeepSkyBlue4")

  ;; this should be for any form of literal value in code medium contrast.
  (set-face-background 'font-lock-string-face personal-bg-color)
  (set-face-foreground 'font-lock-string-face "grey50")

  (set-face-background 'font-lock-constant-face personal-bg-color)
  (set-face-foreground 'font-lock-constant-face "grey50")

  (set-face-background 'font-lock-constant-face personal-bg-color)
  (set-face-foreground 'font-lock-constant-face "grey50")

  (set-face-background 'font-lock-warning-face personal-bg-color)
  ;;(set-face-attribute  'font-lock-warning-face :underline "red")
  (set-face-foreground 'font-lock-warning-face "grey70"))

(defconst grail-micparen-background "grey10")

(defun display-mic-paren ()
  (grail-ignore
    "Load and configure mic-paren"
    "configure mic-paren which performs background highlighting"

    (require 'mic-paren)
    (paren-activate)

    (set-face-background 'paren-face-match grail-micparen-background)
    (set-face-foreground 'paren-face-match "orange3")

    (set-face-background 'paren-face-mismatch grail-micparen-background)
    (set-face-foreground 'paren-face-mismatch "red")

    (set-face-background 'paren-face-no-match grail-micparen-background)
    (set-face-foreground 'paren-face-no-match "DarkCyan") ))

(defun display-faces-for-cperl ()
  (setq cperl-invalid-face nil)

  (set-face-background 'cperl-array-face personal-bg-color)
  (set-face-foreground 'cperl-array-face "aquamarine3")

  (set-face-background 'cperl-hash-face personal-bg-color)
  (set-face-foreground 'cperl-hash-face "aquamarine3")

  (set-face-background 'cperl-nonoverridable-face personal-bg-color)
  (set-face-foreground 'cperl-nonoverridable-face "DeepSkyBlue4") )

(defun display-faces-for-ediff ()
  "display-faces-for-ediff

   set the ediff graphical colors
  "
  (interactive)
  (let
    ((diff-bg-color "black")

     (diff-selected-bg "steel blue")
     (diff-selected-fg "gold")

     (diff-odd-bg "dark slate grey")
     (diff-odd-fg "black")

     (diff-even-bg "dark slate grey")
     (diff-even-fg "black")

     (diff-fine-bg "linkColor")
     (diff-fine-fg "black")

     (diff-merge-bg "turquoise3")
     (diff-merge-fg "black"))

    (set-face-background 'ediff-current-diff-A diff-selected-bg)
    (set-face-foreground 'ediff-current-diff-A diff-selected-fg)

    (set-face-background 'ediff-current-diff-B diff-selected-bg)
    (set-face-foreground 'ediff-current-diff-B diff-selected-fg)

    (set-face-background 'ediff-current-diff-C diff-selected-bg)
    (set-face-foreground 'ediff-current-diff-C diff-selected-fg)

    (set-face-background 'ediff-even-diff-A diff-even-bg)
    (set-face-foreground 'ediff-even-diff-A diff-even-fg)

    (set-face-background 'ediff-even-diff-B diff-even-bg)
    (set-face-foreground 'ediff-even-diff-B diff-even-fg)

    (set-face-background 'ediff-even-diff-C diff-even-bg)
    (set-face-foreground 'ediff-even-diff-C diff-even-fg)

    (set-face-background 'ediff-odd-diff-A diff-odd-bg)
    (set-face-foreground 'ediff-odd-diff-A diff-odd-fg)

    (set-face-background 'ediff-odd-diff-B diff-odd-bg)
    (set-face-foreground 'ediff-odd-diff-B diff-odd-fg)

    (set-face-background 'ediff-odd-diff-C diff-odd-bg)
    (set-face-foreground 'ediff-odd-diff-C diff-odd-fg)

    (set-face-background 'ediff-fine-diff-A diff-fine-bg)
    (set-face-foreground 'ediff-fine-diff-A diff-fine-fg)

    (set-face-background 'ediff-fine-diff-B diff-fine-bg)
    (set-face-foreground 'ediff-fine-diff-B diff-fine-fg)

    (set-face-background 'ediff-fine-diff-C diff-merge-bg)
    (set-face-foreground 'ediff-fine-diff-C diff-merge-fg)

    (setq-default ediff-split-window-function 'split-window-vertically)
    (setq-default ediff-merge-split-window-function 'split-window-vertically) ))

(defun display-faces-for-whitespace-mode ()
  (set-face-background 'whitespace-tab "red")
  (set-face-foreground 'whitespace-tab "yellow")

  (set-face-attribute 'whitespace-tab nil :underline t)
  (set-face-attribute 'whitespace-tab nil :inverse-video nil)

  (set-face-background 'whitespace-trailing personal-bg-color)
  (set-face-foreground 'whitespace-trailing "yellow")

  (set-face-attribute 'whitespace-trailing nil :underline t)
  (set-face-attribute 'whitespace-trailing nil :inverse-video nil))

(defun display-faces-for-helm ()
  (set-face-background 'helm-source-header "black")
  (set-face-foreground 'helm-source-header "DeepSkyBlue4")

  (set-face-background 'helm-visible-mark "black")
  (set-face-foreground 'helm-visible-mark "orange1")

  (set-face-background 'helm-header "black")
  (set-face-foreground 'helm-header "LightSteelBlue")

  (set-face-background 'helm-candidate-number "black")
  (set-face-foreground 'helm-candidate-number "firebrick1")

  (set-face-background 'helm-selection "grey20")
  (set-face-foreground 'helm-selection "LightSteelBlue"))

(defun display-faces-for-paren ()
  (set-face-background 'paren-face-match "grey15"))

(defun display-faces-for-flyspell ()
  (set-face-attribute  'flyspell-incorrect nil :underline t)
  (set-face-foreground 'flyspell-incorrect "red"))

(defun display-faces-for-term ()
  (set-face-foreground 'term "DarkOrange2") )

(defun display-faces-graphical ()
  (require 'cperl-mode)
  (display-faces-for-cperl)

  (require 'ediff)
  (display-faces-for-ediff)

  (require 'whitespace)
  (display-faces-for-whitespace-mode)

  (require 'helm)
  (display-faces-for-helm)

  (require 'mic-paren)
  (display-faces-for-paren)

  (require 'flyspell)
  (display-faces-for-flyspell)

  (require 'term)
  (display-faces-for-term) )

(defun grail-load-graphical (frame)
  "grail-load-display

   load/reload the graphical display configuration
  "
  (when (is-frame-gui frame)
    (setq-default
      use-dialog-box nil
      cursor-type 'hollow)

    ;; this never worked as a frame parameter.
    (set-cursor-color "yellow")

    (blink-cursor-mode 0)

    (setq grail-graphical-config (grail-graphical-frame-configuration frame))

    (setq default-frame-alist grail-graphical-config)

    (grail-set-font (grail-build-font frame))
    (grail-set-transparency grail-transparency)
    (grail-default-transparency grail-transparency)

    (display-faces-graphical)

    (add-hook 'after-make-frame-functions
      (lambda (frame)
        (grail-set-font (grail-build-font frame))) )

    (set-face-background 'default personal-bg-color)
    (set-face-foreground 'default "grey55")

    (display-faces-general)
    (display-mic-paren) ))

(defun grail-reload-graphical ()
  "command to reload the graphical configuration"
  (interactive)
  (grail-load-graphical (selected-frame)) )

(defun grail-set-font (font-spec)
  (interactive "sFont Spec \"<family> <size>\": ")

  (set-frame-font font-spec nil t t))

(defun grail-set-transparency (percent)
  (interactive "nEnter Percent: ")
  (set-frame-parameter (selected-frame) 'alpha `(,percent . ,percent)))

(defun grail-default-transparency (percent)
  (grail-set-transparency percent)
  (add-to-list 'default-frame-alist (list 'alpha percent percent)) )

(defun show-fonts ()
  (interactive)
  (message (concat "available fonts: " (string-join (font-family-list (selected-frame))))))

(defun set-completed-font (font)
  (interactive (list (completing-read "Font: " (font-family-list (selected-frame)))))
  (let
    ((size (read-number "Size: " 26)))

    (grail-set-font (concat font "-" (number-to-string size)))  ))

(provide 'graphical-display)

