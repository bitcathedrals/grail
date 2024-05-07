;; -*-no-byte-compile: t; -*-

(require 'restclient)

(require 'borg-repl)
(require 'buffer-ring)
(require 'programming-generic)

(defconst restclient/ring-name "http")

(setq auto-mode-alist
  (append '(("\\.http$"    . restclient-mode)) auto-mode-alist))

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

  (buffer-ring/add restclient/ring-name)
  (buffer-ring/local-keybindings)

  (programmming-mode-generic 'http-methods))

(add-hook 'restclient-mode-hook 'setup-restclient)
