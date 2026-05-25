;; -*-no-byte-compile: t; -*-

;;
;; FreeBSD configuration file
;;

;; fix the delete key
(global-set-key (kbd "C-h") 'delete-backward-char)

;; Enable loopback so that pinentry will pop up in emacs
(setq
  epg-pinentry-mode 'loopback)

(setq
  grail-font-family '("Hack" "Cousine" "Spleen" "DejaVu Sans Mono" "Courier New")
  grail-font-size 14)
