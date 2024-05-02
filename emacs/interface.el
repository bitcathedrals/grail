;; -*-no-byte-compile: t; -*-

(require 'custom-key)

;; get rid of that horrible dinging
(setq ring-bell-function 'ignore)          ;; not only is this annoying it
                                           ;; triggers Emacs rendering bugs

;; disable things I don't use from eating screen-space

(when (display-graphic-p)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (menu-bar-mode -1) )

(toggle-uniquify-buffer-names)

(require 'mega-modeline)
(setup-mega-modeline)

(setq
  initial-scratch-message nil
  inhibit-splash-screen t
  inhibit-startup-echo-area-message t

  global-font-lock-mode t)

(transient-mark-mode -1)

(setq-default set-mark-command-repeat-pop t)  ;; C-u C-<spc> pops the mark, with this on
                                              ;; simply repeating C-<spc> continues backwards
                                              ;; through the ring. makes it easier to rewind back
                                              ;; through marks.

(fset 'yes-or-no-p 'y-or-n-p)                 ;; y/n instead of yes/no

(put 'erase-buffer 'disabled nil)             ;; enable erase-buffer, no hand-holding.

(defun reg-insert-buffer ()
  "insert the current buffer into a register"
  (interactive)
  `(buffer .  ,(current-buffer)) )

(custom-key-group "registers" "r" t
  ("v" . view-register)
  ("c" . copy-to-register)
  ("a" . append-to-register)
  ("i" . insert-register)
  ("s" . point-to-register)
  ("j" . jump-to-register)
  ("b" . reg-insert-buffer) )

(defun neofetch ()
  "neofetch

  insert neofetch into a buffer"
  (interactive)

  (ansi-term "neofetch" "neofetch")

  (pop-to-buffer buffer))

(setq-default auto-revert-verbose nil)

(defun log-mode-enable-p (path)
  (let
    ((extension (file-name-extension path)))

    (and (stringp extension) (string-equal "log" extension)) ))

(defun log-mode-auto ()
  "log-mode-auto

   automatically turn on log-mode from find-file
  "
  (interactive)
  (when (log-mode-enable-p buffer-file-name)
    (call-interactively 'log-mode-on)
    (buffer-status-add "log-mode-on for log file") ))

(defun log-mode-on ()
  "log-mode-on

   turn on view-mode and auto-revert-tail-mode"
  (interactive)

  (view-mode-enter)
  (auto-revert-tail-mode)

  (keymap-local-set "j" 'pop-to-json) )

(defun log-mode-off ()
  "log-mode-off

   turn off log mode
  "
  (interactive)

  (view-mode-exit)
  (auto-revert-tail-mode))

(add-hook 'find-file-hook 'log-mode-auto)

(defun pop-to-json (beginning end)
  "pop-to-json

   given a region pop to a pretty-printed json buffer
  "
  (interactive "r")

  (let
    ((json-region (buffer-substring beginning end)))

    (with-current-buffer (get-buffer-create "*json*")
      (erase-buffer)
      (insert json-region)
      (json-pretty-print (point-min) (point-max))

      (keymap-local-set "q" 'keybindings-help-quit)

      (pop-to-buffer (current-buffer)) )) )

