;; -*-no-byte-compile: t; -*-

(require 'user-browser)

(defvar-local documentation-buffer-name nil)
(defvar-local documentation-ring-name nil)
(defvar-local documentation-browse-fn nil)

(defvar-local documentation-browse-default-url nil)
(defvar-local documentation-browse-current-url nil)

(defun code-documentation-browse-popup ( url )
  (let
    ((original-buffer (current-buffer)))

    (setq documentation-browse-current-url url)
    (browser-profile-make-unique documentation-buffer-name documentation-ring-name)

    (funcall browser-profile-url-command url)

    (switch-to-buffer original-buffer)
    (pop-to-buffer documentation-buffer-name nil t)

    (other-window-non-interactive)) )

(defun code-documentation-browse-action ( &optional url )
  (interactive)
  (if (get-buffer documentation-buffer-name)
    (if (string-equal url documentation-browse-current-url)
      (pop-to-buffer documentation-buffer-name)
      (progn
        (kill-buffer (get-buffer documentation-buffer-name))
        (code-documentation-browse-popup (or url documentation-browse-default-url))) )

    (code-documentation-browse-popup (or url documentation-browse-default-url)) ))

(defun code-documentation-setup ( buffer-name ring-name default-url &optional browser-fn )
  (make-variable-buffer-local 'browse-url-browser-function)

  (setq
    documentation-buffer-name buffer-name
    documentation-ring-name ring-name
    documentation-browse-default-url default-url

    documentation-browse-fn (or browser-fn browser-profile-url-command)

    browse-url-browser-function 'code-documentation-browse-action)

  (configure-for-docs 'code-documentation-browse-action))

(provide 'user/code-documentation)
