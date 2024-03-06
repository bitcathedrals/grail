;; -*-no-byte-compile: t; -*-

;;----------------------------------------------------------------------
;; macos.el
;;----------------------------------------------------------------------
(let
  ((brew-dir (concat (getenv "HOME") "/homebrew/")))

  (mapcar
    (lambda ( bin-dir )
      (if (file-directory-p (concat brew-dir bin-dir))
        (setq exec-path (cons (concat brew-dir bin-dir) exec-path)) ))
    '("bin" "sbin")) )

;; emacs gets trashed if there is no font specified through
;; the grail system

(setq
  grail-font-family '("Cousine" "Hack" "Spleen" "DejaVu Sans Mono" "Courier New")
  grail-font-size 22
  grail-transparency 90)

(setq exec-path (seq-uniq (append
                            '("/Applications/Emacs.app/Contents/MacOS/bin"
                              "/usr/local/bin")
                          exec-path)) )

;;
;; set brew correctly
;;
(let
  ((brew-prefix (string-trim-right (shell-command-to-string "brew --prefix")) ))

  (setq exec-path (seq-uniq (append
                              (list (concat brew-prefix "/bin") (concat brew-prefix "/sbin"))
                              exec-path) )) )

