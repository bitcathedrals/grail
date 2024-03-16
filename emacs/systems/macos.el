;; -*-no-byte-compile: t; -*-

;;----------------------------------------------------------------------
;; macos.el
;;----------------------------------------------------------------------
(require 'woman)

(setq
 user-brew (concat (getenv "HOME") "/homebrew/")
 user-local "/usr/local/"

 bin-dirs '("bin" "sbin")
 man-dirs '("/share/man"))

(defun setup-macos-paths ()
  (interactive)

  (maybe-add-path "/Applications/Emacs.app/Contents/MacOS/bin" 'exec-path)

  (when (file-directory-p user-brew)
    (map-paths user-brew bin-dirs 'exec-path)
    (map-paths user-brew man-dirs 'woman-path))

  (when (file-directory-p user-local)
    (map-paths user-local bin-dirs 'exec-path)
    (map-paths user-local man-dirs 'woman-path)) )

(setup-macos-paths)

;; emacs gets trashed if there is no font specified through
;; the grail system. Im on big displays so crank up the font size.

(setq
  grail-font-family '("Cousine" "Hack" "Spleen" "DejaVu Sans Mono" "Courier New")
  grail-font-size 22
  grail-transparency 90)


