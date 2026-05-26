;; -*-no-byte-compile: t; -*-

(require 'eat)
(require 'term)
(require 'buffer-ring)
(require 'custom-key)

(defconst user-terminal-name "shell")

(setq grail-terminal "vt100")

(setenv "TERM" grail-terminal)

;;----------------------------------------------------------------------
;;                 IPC shell:  comint/term mode
;;----------------------------------------------------------------------

(defadvice term-handle-exit (after terminal-profile/handle-exit)
  "Kills the current buffer when the term process exits."
  (message "process exited. killing buffer.")
  (kill-buffer (current-buffer)))

(ad-activate 'term-handle-exit)

(setq explicit-shell-file-name "zsh"
      explicit-zsh-args '("-i"))

(defun user-terminal-ring ()
  (buffer-ring/add user-terminal-name)
  (buffer-ring/local-keybindings))

(defadvice term (after terminal-profile/term-bindings)
  (user-terminal-ring))

(defadvice ansi-term (after terminal-profile/ansi-bindings)
  (user-terminal-ring))

(ad-activate 'term)
(ad-activate 'ansi-term)

(defun full-term ()
  "run a full terminal in full window C-u or (default) split window"
  (interactive)

  (split-window-below)
  (other-window 1)

  (term explicit-shell-file-name))

(defun shell-term ( prefix &optional command )
  "run a local shell in full window C-u or (default) split window"
  (interactive "P")

  (unless (and prefix (equal (car prefix) 4))
    (split-window-horizontally)
    (other-window 1))

  (ansi-term
    (if command
      command
      terminal-profile-local-shell)) )

;; setup shell-mode in case I use it
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(add-to-list 'comint-output-filter-functions 'ansi-color-process-output)

;; tmux

(defun tmux ()
  "tmux

   start a tmux session
  "
  (interactive)

  (eat "tmux"))

;; zsh shell

(defun hungry ()
  (interactive)
  (message "C-c C-e for buffer mode, C-c C-j for character mode.")
  (eat "zsh"))

(defun zapper ()
  (interactive)
  (message "C-c C-e for buffer mode, C-c C-j for character mode.")
  (eat "lldb"))

;; eat

(setq eat-term-name grail-terminal)

(defun setup-eat-general ()
  (interactive)

  (setq-local process-adaptive-read-buffering nil))

(add-hook 'eat-mode-hook 'setup-eat-general)

