;;----------------------------------------------------------------------
;; user.el - user interface configuration
;;
;; description:
;;
;; configuration of core emacs functionality.
;;----------------------------------------------------------------------
(require 'buffer-ring)

;;----------------------------------------------------------------------
;; crypto
;;----------------------------------------------------------------------
(require 'password-cache)

(setq
  password-cache t
  password-cache-expiry nil)

;;----------------------------------------------------------------------
;; emacs enhancements
;;----------------------------------------------------------------------
(require 'dwim-tab)
(require 'buffer-ring)

;;----------------------------------------------------------------------
;; utilities
;;----------------------------------------------------------------------
(require 'ext-logging)

;;----------------------------------------------------------------------
;; general grail profiles
;;----------------------------------------------------------------------

;; registers

(use-grail-profiles 0 "dwim-complete" "net-paste")

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
(setq auto-mode-alist (append '(("\\.txt$"     . text-mode)) auto-mode-alist ))

;----------------------------------------------------------------------
;;                    EShell
;;----------------------------------------------------------------------

(require 'eshell)

(setq
  eshell-windowed t                ;; enable windowing
  eshell-save-history-on-exit nil) ;; kill the prompt to save history

(add-hook 'eshell-mode-hook
  (lambda ()
    ;; add a list of commands that will pop a term buffer for out-of-eshell
    ;; handling. Note: the variable eshell-visual-commands is buffer-local.
    (setq eshell-visual-commands
      (append eshell-visual-commands (list "ssh" "su" "telnet"
                                           "ftp" "lftp" "links")))

    ;; I rarely want to quit eshell. when I do I can use quit. map
    ;; the usual kill-buffer keybinding to rid-window.
    (local-set-key (kbd "C-x k") 'rid-window))
  t)

;;----------------------------------------------------------------------
;;                    ERC
;;----------------------------------------------------------------------

;; emacs IRC client is handy when on #emacs ...

(setq
  erc-default-server "irc.libera.chat"
  erc-default-port "6667"
  erc-nick "JohnGalt")

;; turn on truncate mode before erc eats all available RAM.
(require 'erc-truncate)
(erc-truncate-mode 1)

(setq erc-prompt-for-nickserv-password nil)

(require 'erc-credentials)

(setq erc-network-hide-list '(("Libera.Chat" "JOIN" "PART" "QUIT")))

(defun erc-mode-customization ()
  (buffer-ring/add "erc")
  (buffer-ring/local-keybindings)

  (local-set-key (kbd "<up>") 'erc-previous-command) )

(add-hook 'erc-mode-hook 'erc-mode-customization t)

;;----------------------------------------------------------------------
;; ido smart completion
;;----------------------------------------------------------------------
(require 'ido)

(setq ido-enable-flex-matching t)
(setq ido-use-filename-at-point 'guess)

(ido-mode 1)

;;----------------------------------------------------------------------
;; read/write perm handling
;;----------------------------------------------------------------------
(require 'rw-utilities)

(setq auto-mode-alist
  (cons '("\\.firewall$" . conf-mode) auto-mode-alist) )

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
