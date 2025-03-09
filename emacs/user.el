;; -*- lexical-binding: t; no-byte-compile: t; -*-

(require 'buffer-ring)

(setq warning-minimum-level :emergency)

(require 'dwim-tab)

;;----------------------------------------------------------------------
;; general grail profiles
;;----------------------------------------------------------------------

;; registers

(use-grail-profiles 0 "dwim-complete" "net-paste" "search")

(use-grail-profiles 10 "activate-buffer-status")

;;----------------------------------------------------------------------
;;                    General Modifications
;;----------------------------------------------------------------------

(require 'proced)

;; disable customization, automatic persistence of configuration changes.
;; I personally don't like customize as I prefer emacs to start with
;; a state I have personally defined and reviewed.

;; this line is a nasty way of disabling customize, simply specify the
;; customize file as /dev/null.

(setq custom-file "/dev/null")

;; basic settings

;;
;; buffer coding and line ending handling
;;
;; unix eol, utf-8 coding, and tab character insertion
;;
(setq
  buffer-file-coding-system 'utf-8-unix
  set-language-environment "UTF-8"
  set-locale-environment "en_US.UTF-8")

(setq file-coding-system-alist '( (".*" . utf-8-unix) ))

(prefer-coding-system 'utf-8-unix)

(require 'eol-utilities)

;;----------------------------------------------------------------------
;; whitespace handling. tabs are evil.
;;----------------------------------------------------------------------
(require 'whitespace-utilities)

;;----------------------------------------------------------------------
;; lots of defaults that will be broken out when they become more than
;; setting a single flag.
;;----------------------------------------------------------------------

(setq
  make-backup-files nil

  ;; protect against IO errors by writing to a temp file and then renaming
  ;; so the original is not trashed by partial writes.
  file-precious-flag t

  ;; keep woman from making a frame.
  woman-use-own-frame nil

  ;; some programs fail without a newline terminator
  require-final-newline t

  ;; when traversing sexp's ignore comments
  parse-sexp-ignore-comments t

  ;; files can have elisp statements that are executed when the
  ;; file is loaded. My paranoia says hell no.
  enable-local-eval nil)

;;
;; associate major modes with file extensions.
;;
(setq auto-mode-alist (append '(("\\.txt\\'"     . text-mode)) auto-mode-alist))

;;
;;                    ERC
;;
(require 'erc)
(require 'erc-sasl)

(require 'sensitive)

(setq
  erc-default-server "irc.libera.chat"
  erc-default-port "6667"
  erc-prompt-for-nickserv-password nil
  erc-network-hide-list '(("Libera.Chat" "JOIN" "PART" "QUIT")) )

(add-to-list 'erc-modules 'sasl)

(defun erc-mode-customization ()
  (buffer-ring/add "erc")
  (buffer-ring/local-keybindings)

  (keymap-local-set "<up>" 'erc-previous-command))

(add-hook 'erc-mode-hook 'erc-mode-customization t)

(defun erc-sasl-connect ()
  "ERC-SASL-CONNECT
   make a SASL erc-tls connection
  "
  (interactive)

  (with-temp-buffer
    (let*
      ((serv-info (auth-source-search
                    :host "sasl.libera.chat"
                    :require '(:user :secret)))
       (auth-info (auth-source-search
                    :host "irc.libera.chat"
                    :require '(:user :secret)))
       (server-id (plist-get (car serv-info) :user))

       (erc-id    (plist-get (car auth-info) :user))
       (erc-pass  (funcall (plist-get (car auth-info) :secret))) )

    (erc-tls :server   erc-default-server
             :user     server-id
             :nick     erc-id
             :password erc-pass
             :full-name "John Galt") )) )

(require 'erc-services)
(erc-services-mode 1)

;;
;; helm completion
;;
(require 'helm-mode)

(require 'helm-files)
(require 'helm-buffers)
(require 'helm-grep)
(require 'helm-occur)
(require 'helm-regexp)
(require 'helm-man)
(require 'helm-ring)
(require 'helm-frame)

(global-set-key (kbd "C-x C-f") 'helm-find-files)

(custom-key-group "complete" "c" t
  ("f" . helm-find-files)
  ("b" . helm-buffers-list)
  ("g" . helm-grep-do-git-grep)
  ("o" . helm-occur)
  ("r" . helm-register)
  ("s" . helm-regexp)
  ("m" . helm-man-woman)
  ("k" . helm-show-kill-ring))

(defun helm-on-frames ()
  (interacive)

  (add-hook 'helm-after-action-hook 'helm-frame-delete)
  (add-hook 'helm-cleanup-hook 'helm-frame-delete)

  (setq helm-split-window-preferred-function 'switch-to-helm-frame))

(defun switch-to-helm-frame()
  "switch-to-helm-frame

    automatically switch to the frame created by helm
   "
  (select-frame (helm-frame-window) t))

(setq auto-mode-alist
  (cons '("\\.firewall\\'" . conf-mode) auto-mode-alist))

;;
;; dpaste
;;
(require 'dpaste)
(setq dpaste-poster "Anonymous")

;;
;; force spaces over tabs
;;
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)

;; (mapcar 'buffer-name (buffer-list))

(defun eat-insert-buffer (buffer-name)
  "insert-buffer-clean

   insert the buffer clean of text properties
  "
  (interactive (list
                 (completing-read
                   "insert buffer: "
                   (mapcar 'buffer-name (buffer-list)) )))

  (let
    ((extracted ""))

    (with-current-buffer (get-buffer buffer-name)
      (setq extracted (buffer-substring-no-properties (point-min) (point-max))) )

    (eat-term-send-string eat-terminal extracted) ))


