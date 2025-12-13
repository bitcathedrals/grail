;; -*-no-byte-compile: t; -*-

(defconst npaste-default-name user-login-name)

;; (require 'webpaste)
;; (setq webpaste-provider-priority '(dpaste.org))

(require 'request)

(defun bpa.st-post (data)
  (request "http://bpa.st:8000"
    :type "POST"
    :data data
    :success (cl-function
               (lambda (&key data &allow-other-keys)
                 (message "I sent: %S" data)))
    :error (cl-function
             (lambda (&key status-code &allow-other-keys)
               (error "HTTP Error: %s" status-code)))) )

;;
;; net-paste interface
;;

(defun npaste-default-title ()
  (concat npaste-default-name "/"
          (format-time-string "%a(%H:%M:%S)" (current-time))) )

(defun npaste-title ()
  (let
    ((paste-title (read-string "title? ")))

    (if (< (length paste-title) 1)
      (npaste-default-title)
      (concat (npaste-default-title) "-" paste-title)) ))

(defun npaste-region ()
  (interactive)
  (bpa.st-post (buffer-substring-no-properties (point) (mark))))

(defun npaste-buffer ()
  (interactive)
  (bpa.st-post (buffer-substring-no-properties (point-min) (point-max))))

(provide 'profile/paste)
