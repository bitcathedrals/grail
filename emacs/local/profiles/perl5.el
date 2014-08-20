;;----------------------------------------------------------------------
;; perl5
;;----------------------------------------------------------------------
(require 'cperl-mode)

(require 'dwim-tab)
(require 'mode-tools)
(require 'borg-repl)

(require 'programming-generic)

(remap-assoc-mode-to 'perl-mode 'cperl-mode)

(defconst profile/perl5-mode-name "perl5")

(setq
  cperl-indent-parens-as-block t

  cperl-indent-level 2
  cperl-continued-statement-offset 2

  cperl-close-paren-offset -2

  cperl-indent-subs-specially nil

  cperl-highlight-variables-indiscriminately t

  cperl-electric-parens nil)

(defconst profile/perl5-function-regex "sub")

(defun profile/perl5-fn-signatures ()
  (interactive)
  (occur perl-function-regex))

(defun profile/perl5-mode-setup ()
  (set-face-foreground cperl-pod-face "orange3")

  (programming-mode-generic 'profile/perl5-fn-signatures)

  (buffer-ring/add profile/perl5-mode-name)
  (buffer-ring/local-keybindings)

  (turn-on-dwim-tab)

  (grail-require profile/syntax-tools
    "perl profile"
    "smart syntax"
    (profile/syntax-tools-mode-setup) ) )

(add-hook 'cperl-mode-hook 'profile/perl5-mode-setup t)

(grail-load-package 'pde "git" "https://github.com/wenbinye/emacs-pde.git")

;; be careful about pde-load which is the usual way to load. triggers
;; transient mark amongst other things.
(strip-minor-mode-keymap 'pde-mode)

(require 'inf-perl)

(defconst profile/perl5-repl-name (borg-repl/repl-name profile/perl5-mode-name))

;; tried perlconsole because it was readline.

(setq pde-perl-program "perlconsole")

(defun profile/perl5-repl-setup ()

  ;; no actual send buffer so use send
  ;; region as a dummy function.
  (borg-repl/bind-repl profile/perl5-repl-name
    'run-perl
    'inf-perl-send
    'inf-perl-send-region
    'inf-perl-send-region) )

(add-hook 'cperl-mode-hook 'profile/perl5-repl-setup t)

(provide 'profile/perl5)
