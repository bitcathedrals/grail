;;----------------------------------------------------------------------
;; visual.el
;; Primary Author: Mike Mattie
;;----------------------------------------------------------------------

;; the visual configuration for Emacs that is quite idiosyncratic.

(defun simple-set-expand-prop ( set-op spec )
  "beta"
  (if (string-equal (symbol-name set-op) "splice")
    ;; when multiple properties are specified splice them in.
    spec

    ;; when a single property is specified create the property
    ;; syntax. The property name is constructed with read. The
    ;; specification is eval'd to render it essentially
    ;; typeless. Symbols now require quoting.
    (list (read (concat ":" (symbol-name set-op))) (eval spec))))

(defmacro simple-set-theme ( theme &rest given-forms )
  "create the syntax for custom-theme-set-faces"
  (append
    `(custom-theme-set-faces ',theme)

     (mapcar
       (lambda (form)
         `'(,(car form)
             ((t
                ,(simple-set-expand-prop (cadr form) (caddr form))
                )))
         ) given-forms)
    ))

(simple-set-theme user
  ;; default
  (default splice (:stipple nil
                    :background "black"
                    :foreground "grey70"
                    :inverse-video nil
                    :box nil
                    :strike-through nil
                    :overline nil
                    :underline nil
                    :slant normal
                    :weight normal
                    :height 110
                    :width normal
                    :family "Courier New"))

  ;; comments are set off-tempature to distingiush them better.
  ;; orange was chosen as a red that wasn't harsh.
  (font-lock-comment-face foreground "orange3")

  ;; language syntax is the darkest shade of blue
  (font-lock-keyword-face foreground "DeepSkyBlue4")
  (cperl-nonoverridable-face foreground "DeepSkyBlue4")

  ;; grammar is the lightest shade of blue
  (font-lock-builtin-face foreground "SkyBlue3")

  (paren-face-match-light background "grey20")
  (paren-face-match foreground "red")

  ;; this should be for any form of literal value in code medium contrast.
  (font-lock-string-face  foreground "grey50")
  (font-lock-constant-face foreground "grey50")

  ;; decl is dark green
  (font-lock-type-face foreground "green4")
  (font-lock-function-name-face foreground "aquamarine4")
  (font-lock-variable-name-face foreground "aquamarine3")

  ;; perl misc
  (cperl-array-face slant 'italic)
  (cperl-hash-face  slant 'italic)

  ;; flyspell.
  (flyspell-incorrect underline t)
)