;; -*-no-byte-compile: t; -*-

(require 'restclient)

(require 'borg-repl)
(require 'buffer-ring)
(require 'programming-generic)

(setq auto-mode-alist
  (append '(("\\.http\\'"    . restclient-mode)) auto-mode-alist))

(setq
  rest-client-buffer-response-name "*REST*")

(defun http-methods ()
  "shell-mode-functions

   occur all the functions in a shell mode buffer
  "
  (interactive)
  (occur "^\(GET|PUT|POST|DELETE\).*"))

(defun setup-restclient ()
  (interactive)

  (programmming-mode-generic nil 'http-methods))

(add-hook 'restclient-mode-hook 'setup-restclient)
