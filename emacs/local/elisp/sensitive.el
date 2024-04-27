;; -*-no-byte-compile: t; -*-

(require 'erc)

(setq
  erc-nick "JohnGalt"
  erc-ignore-list '("whateverdude" "quiliro" "grym"))

;;
;; erc setup
;;

(defun nickserv-identify ()
  (erc-send-input "/msg NickServ IDENTIFY JohnGalt Chavez#Code#Monkey"))

(setq erc-after-connect
  (cons
    (lambda (server nick)
      (nickserv-identify))
    erc-after-connect))

(setq
  epg-user-id "codermattie@runbox.com")

(provide 'sensitive)

