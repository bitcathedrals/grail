;; -*-no-byte-compile: t; -*-

;;----------------------------------------------------------------------
;; tramp setup for emacs.
;;----------------------------------------------------------------------

(require 'tramp)

; tramp-chunksize is defensive to reduce problems with hangs from sending
; to large of chunks

(setq-default
  tramp-shell-prompt-pattern  ".*>"
  tramp-default-method "ssh"
  tramp-chunksize 500)

(setq
 tramp-ssh-controlmaster-options
 (concat
   "-o ControlPath=~/.ssh/%%r@%%h:%%p "
   "-o ControlMaster=auto -o ControlPersist=yes "))

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

(defconst ssh-host-list '("work"
                          "personal"
                          "gatekeeper"
                          "git"
                          "pypi"
                          "builder"
                          "devil"))

(defun ssh-tramp (host)
  (interactive (list (completing-read "ssh [tramp]: " ssh-host-list)))
  (message "attempting to connect to host: %s" host)
  (call-interactively 'find-file (format "/ssh:%s:~" host)) )

(defun ssh-host (host)
  (interactive (list (completing-read "ssh [shell]: " ssh-host-list)))
  (let
    ((command (concat "ssh " host)))

    (with-current-buffer (call-interactively 'eat command t)
      (rename-buffer host)) ))
