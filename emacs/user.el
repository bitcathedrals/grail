;; -*-no-byte-compile: t; -*-

(require 'buffer-ring)

(setq warning-minimum-level :emergency)

;;----------------------------------------------------------------------
;; emacs enhancements
;;----------------------------------------------------------------------
(require 'dwim-tab)
(require 'buffer-ring)

;;----------------------------------------------------------------------
;; general grail profiles
;;----------------------------------------------------------------------

;; registers

(use-grail-profiles 0 "dwim-complete" "net-paste" "search")

(use-grail-profiles 10 "activate-buffer-status")

;;----------------------------------------------------------------------
;;                    General Modifications
;;----------------------------------------------------------------------

;; disable customization, automatic persistence of configuration changes.
;; I personally don't like customize as I prefer emacs to start with
;; a state I have personally defined and reviewed.

;; this line is a nasty way of disabling customize, simply specify the
;; customize file as /dev/null.

(setq custom-file "/dev/null")

;; basic settings

;;----------------------------------------------------------------------
;; buffer coding and line ending handling
;;
;; unix eol, utf-8 coding, and tab character insertion
;;----------------------------------------------------------------------
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

;;----------------------------------------------------------------------
;; associate major modes with file extensions.
;;----------------------------------------------------------------------
(setq auto-mode-alist (append '(("\\.txt$"     . text-mode)) auto-mode-alist))

;;----------------------------------------------------------------------
;;                    ERC
;;----------------------------------------------------------------------
(require 'erc)
(require 'erc-truncate)

(grail-try-elisp "local/elisp/erc-sensitive.el")

(require 'sensitive)

(setq
  erc-default-server "irc.libera.chat"
  erc-default-port "6667"
  erc-prompt-for-nickserv-password nil
  erc-network-hide-list '(("Libera.Chat" "JOIN" "PART" "QUIT")) )

;; turn on truncate mode before erc eats all available RAM.
(erc-truncate-mode 1)

(defun erc-mode-customization ()
  (buffer-ring/add "erc")
  (buffer-ring/local-keybindings)

  (local-set-key (kbd "<up>") 'erc-previous-command) )

(add-hook 'erc-mode-hook 'erc-mode-customization t)

;;----------------------------------------------------------------------
;; helm completion
;;----------------------------------------------------------------------
(require 'helm-mode)

(require 'helm-files)
(require 'helm-buffers)
(require 'helm-grep)
(require 'helm-occur)
(require 'helm-regexp)
(require 'helm-man)
(require 'helm-ring)
(require 'helm-frame)

(custom-key-group "helm complete" "c" t
  ("f" . helm-find-files)
  ("b" . helm-buffers-list)
  ("g" . helm-grep-do-git-grep)
  ("o" . helm-occur)
  ("r" . helm-register)
  ("s" . helm-regexp)
  ("m" . helm-man-woman)
  ("k" . helm-show-kill-ring) )

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
  (cons '("\\.firewall$" . conf-mode) auto-mode-alist))

;;----------------------------------------------------------------------
;; dpaste
;;----------------------------------------------------------------------
(require 'dpaste)
(setq dpaste-poster "JohnGalt")

;;----------------------------------------------------------------------
;; force spaces over tabs
;;----------------------------------------------------------------------
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
