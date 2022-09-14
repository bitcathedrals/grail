;;
;; FreeBSD configuration file
;;

;; fix the delete key
(global-set-key (kbd "C-h") 'delete-backward-char)

;; Enable loopback so that pinentry will pop up in emacs
(setq
  epg-pinentry-mode 'loopback)

