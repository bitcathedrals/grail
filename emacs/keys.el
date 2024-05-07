;; -*-no-byte-compile: t; -*-

(require 'custom-key)
(require 'user-commands)
(require 'ucase-word)

;; remove keybindings
;; (global-unset-key "S-<tab>")

;; this used to be minimize window, now it exits recursive editing
;; which is handy and safer.
(keymap-global-set "C-z"   'top-level)

;; reverse the regex/regular isearch

(keymap-global-set "C-s"   'isearch-forward-regexp)
(keymap-global-set "C-r"   'isearch-backward-regexp)

(keymap-global-set "M-r"   'query-replace-regexp)

;; standard emacs prompt for a interactive command
(keymap-global-set "<escape>"  'execute-extended-command)

;; other window is more useful. there is no really good way
;; for buffer switching outside of buffer ring

(keymap-global-set "<prior>" 'beginning-of-buffer)
(keymap-global-set "<next>"  'end-of-buffer)

(keymap-global-set "M-g"     'goto-line)

(keymap-global-set "M-<tab>" 'other-window-forward)

(keymap-global-set "C-c u"   'toggle-ucase-word)

(defvar user-keys/tree-browser nil)

(defun user-keys/start-tree-browser ()
  "user-keys/start-tree-browser

   start a directory tree browser if one
   has been loaded.
  "
  (interactive)

  (if user-keys/tree-browser
    (let
      (( current-prefix-arg nil ))

      (call-interactively user-keys/tree-browser)
      (funcall user-keys/tree-browser) )
    (message "user-config: no tree browser loaded") ))

(defun pop-dired-in-file ()
  "pop-dired-in-file

   pop a dired buffer in the directory of the current file.
  "
  (interactive)
  (dired-other-window (file-name-directory buffer-file-name)) )

(custom-key-group "files" "f" t
    ("d" . dired)
    ("c" . pop-dired-in-file)
    ("n" . user-keys/start-tree-browser)
    ("s" . save-some-buffers)
    ("b" . hexl-find-file))

(custom-key-group "mark" "m" t
  ("x" . exchange-point-and-mark)
  ("p" . push-mark-command)
  ("g" . pop-global-mark))

(custom-key-group "window" "w" t
  ("v" . split-window-horizontally)
  ("t" . split-window-vertically)
  ("n" . make-frame-command)
  ("d" . delete-frame)
  ("x" . delete-other-windows)
  ("f" . toggle-frame-fullscreen) )

(defvar swap-parens-keymap '(("[" . "(")
                             ("]" . ")")
                             ("(" . "[")
                             (")" . "]")) )

(defun bind-swap-parens ()
  (interactive)

  (mapc (lambda (key-pair) (keymap-global-set (kbd (car key-pair))
                             `(lambda ()
                                (interactive)
                                (insert-char ,(aref (cdr key-pair) 0)) )) )
    swap-parens-keymap))

(bind-swap-parens)
