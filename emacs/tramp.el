;; -*-no-byte-compile: t; -*-

;;----------------------------------------------------------------------
;; tramp profile
;;
;; tramp setup for emacs.
;;----------------------------------------------------------------------

(require 'tramp)

; tramp-chunksize is defensive to reduce problems with hangs from sending
; to large of chunks

(setq-default
  tramp-default-remote-shell "/bin/bash"
  tramp-shell-prompt-pattern  ".*$")

(setq
  tramp-default-method "ssh"
  tramp-chunksize 500)

(defun doas (dir)
  (interactive "Ddoas directory? ")
  (message "attempting to connect to doas: %s" dir)
  (find-file (concat "/doas::" dir)) )

(defun sudo (dir)
  (interactive "Dsudo directory? ")
  (message "attempting to connect to sudo: %s" dir)
  (find-file (concat "/sudo::" dir)) )

;;----------------------------------------------------------------------
;; tramping around
;;----------------------------------------------------------------------
(defun ssh-dired (host)
  (interactive "sEnter Host: ")
  (message "attempting to connect to host %s" host)
  (find-file (format "/ssh:%s:~/" host)) )

(defconst ssh-host-list '("gatekeeper-wifi"
                           "git-wifi"
                           "pypi-wifi"
                           "hades-wifi"
                           "redbox-wifi"
                           "spartan-wifi"
                           "work-wifi"

                           "gatekeeper-eth"
                           "hades-eth"
                           "redbox-eth"
                           "spartan-eth"
                           "work-eth"
                           "gatekeeper-remote"))

(defun ssh-host (host)
  (interactive (list (completing-read "ssh: " ssh-host-list)))
  (let
    ((name (concat "ssh " host)))

    (with-current-buffer (eat name t)
      (rename-buffer host)) ))
