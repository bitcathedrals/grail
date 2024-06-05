;; -*-no-byte-compile: t; -*-

;;----------------------------------------------------------------------
;; spell.el
;;----------------------------------------------------------------------
(require 'flyspell)
(require 'dwim-tab)

(setq-default
  ispell-program-name "aspell"                     ;; use aspell.
  flyspell-issue-message-flag nil)                 ;; don't bog down in bad English.

(unless (executable-find ispell-program-name)
  (grail-signal-fail "grail/spell" "aspell executable not found"))

(defun correct-over-flyspell ()
  "auto-correct the word if over a flyspell region, return t only
   if over a fly-spell region"
  (interactive)

  (if (mode-overlay-at-point-p 'flyspell-overlay)
    (progn
      (flyspell-correct-word-before-point)
      t)) )

;; create a tab context where tab will invoke flyspell-auto-correct-word
;; at the point.

(dwim-tab-globalize-context (dwim-tab-make-expander 'dwim-tab/in-word 'correct-over-flyspell))

