;; -*-no-byte-compile: t; -*-

(require 'erc)

(setq erc-nickserv-passwords
  `((irc.libera.chat
      (("JohnGalt" . "Chavez#Code#Monkey")
       ("TechBroLifer" . "Chavez#Code#Monkey"))) ))

;;
;; erc setup
;;

;; (defun nickserv-identify ()
;;   (erc-send-input "/msg NickServ IDENTIFY JohnGalt Chavez#Code#Monkey"))

;; (setq erc-after-connect
;;   (cons
;;     (lambda (server nick)
;;       (nickserv-identify))
;;     erc-after-connect))

(setq erc-after-connect nil)



(setq
  epg-user-id "codermattie@runbox.com")

(provide 'sensitive)

