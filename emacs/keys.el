;; -*-no-byte-compile: t; -*-
;;----------------------------------------------------------------------
;; keys.el
;;
;; keybinding tools and configuration.
;;----------------------------------------------------------------------
(require 'custom-key)
(require 'user-commands)
(require 'ucase-word)

;; remove keybindings
(global-unset-key (kbd "<S-tab>"))

;; this used to be minimize window, now it exits recursive editing
;; which is handy and safer.
(global-set-key (kbd "C-z")     'top-level)

;; reverse the regex/regular isearch

(global-set-key (kbd "C-s")     'isearch-forward-regexp)
(global-set-key (kbd "C-r")     'isearch-backward-regexp)

(global-set-key (kbd "M-r")     'query-replace-regexp)

;; standard emacs prompt for a interactive command
(global-set-key (kbd "<escape>") 'execute-extended-command)

;; other window is more useful. there is no really good way
;; for buffer switching outside of buffer ring

(global-set-key (kbd "<prior>") 'beginning-of-buffer)
(global-set-key (kbd "<next>")  'end-of-buffer)

(global-set-key (kbd "M-g")  'goto-line)

(global-set-key (kbd "<M-tab>") 'other-window-forward)

;; line number mode
(global-set-key (kbd "C-c C-l")  'linum-mode)

;; registers
(custom-key-group "registers" "r" t
  ("c" . copy-to-register)
  ("i" . insert-register)
  ("l" . list-registers) )

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

(defvar swap-parens-keymap '(("[" . "(")
                             ("]" . ")")
                             ("(" . "[")
                             (")" . "]")) )

(defun bind-swap-parens ()
  (interactive)

  (mapc (lambda (key-pair) (global-set-key (kbd (car key-pair))
                             `(lambda ()
                                (interactive)
                                (insert-char ,(aref (cdr key-pair) 0)) )) )
    swap-parens-keymap))

(bind-swap-parens)
