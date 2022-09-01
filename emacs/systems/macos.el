;;----------------------------------------------------------------------
;; darwin.el
;;
;; configuration for a darwin platform. Make sure you set up the fonts
;; correctly or emacs barfs on the screen.
;;----------------------------------------------------------------------

;; emacs gets trashed if there is no font specified through
;; the grail system
(setq grail-font-family '("DejaVu Sans Mono" "Courier New"))
(setq grail-font-size 160)

(let
  ((user-brew-path (concat (getenv "HOME") "/homebrew/bin")))

  (if (file-exists-p (concat user-brew-path "brew"))
    (setq exec-path (cons user-brew-path exec-path))) )

(setq exec-path (seq-uniq (cons "/usr/local/bin" exec-path)))

;; for some reason when I run emacs out of the dock
;; /usr/local/bin is missing.

