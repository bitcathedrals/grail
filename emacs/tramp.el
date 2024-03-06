;; -*-no-byte-compile: t; -*-

;;----------------------------------------------------------------------
;; tramp profile
;;
;; tramp setup for emacs.
;;----------------------------------------------------------------------

(require 'tramp)

; tramp-chunksize is defensive to reduce problems with hangs from sending
; to large of chunks

(setq
  tramp-default-method "ssh"
  tramp-chunksize 500 )

;;----------------------------------------------------------------------
;; tramping around
;;----------------------------------------------------------------------
(defun ssh-dired (host)
  (interactive "sEnter Host: ")
  (message "attempting to connect to host %s" host)
  (find-file (format "/ssh:%s:~/" host)) )

(defconst ssh-host-list '("gatekeeper-wifi" "git-wifi" "pypi-wifi" "hades-wifi" "redbox-wifi" "spartan-wifi" "work-wifi"
                           "gatekeeper-eth" "hades-eth" "redbox-eth" "spartan-eth" "work-eth" "gatekeeper-remote"))

(defun ssh-host (host)
  (interactive (list (completing-read "ssh: " ssh-host-list)))
  (let
    ((name (concat "ssh " host)))

    (ansi-term name name)) )
