;;----------------------------------------------------------------------
;; macos.el
;;
;; configuration for a darwin platform. Make sure you set up the fonts
;; correctly or emacs barfs on the screen.
;;----------------------------------------------------------------------

;; emacs gets trashed if there is no font specified through
;; the grail system

(setq grail-font-family '("Cousine" "Hack" "Spleen" "DejaVu Sans Mono" "Courier New"))
(setq grail-font-size 28)

(setq exec-path (seq-uniq (append
                            '("/Applications/Emacs.app/Contents/MacOS/bin"
                              "/usr/local/bin")
                          exec-path)) )

;;
;; set brew correctly
;;
(let
  ((brew-prefix (string-trim-right (shell-command-to-string "brew --prefix")) ))

  (setq exec-path (seq-uniq (append
                              (list (concat brew-prefix "/bin") (concat brew-prefix "/sbin"))
                              exec-path) )) )

