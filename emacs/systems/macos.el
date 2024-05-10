;; -*-no-byte-compile: t; -*-

;;----------------------------------------------------------------------
;; macos.el
;;----------------------------------------------------------------------
(require 'woman)
(require 'exec-path-from-shell)

(dolist (var '("SSH_AUTH_SOCK" "SSH_AGENT_PID" "GPG_AGENT_INFO"))
  (add-to-list 'exec-path-from-shell-variables var))

(setq
 user-brew (concat (getenv "HOME") "/homebrew/")
 local-brew "/usr/local/"
 opt-brew "/opt/"

 bin-dirs '("bin" "sbin")
 man-dirs '("/share/man")

 helper-buffer-name "*macos-helper*")

(defun clean-helper-buffer ()
  (let
    ((buffer (get-buffer-create helper-buffer-name)))

    (with-current-buffer buffer
      (erase-buffer))

    buffer))

(defun reuse-helper-buffer ()
  (get-buffer-create helper-buffer-name))

(defun path-to-list (path)
  (let
    ((quote-list (split-string path "\"")))

    (split-string (cadr quote-list) ":")) )

;; (path-to-list "foo=\"foo:bar:baz\" export foo")

(defun macos-path-helper ()
  (interactive)

  (call-process "/usr/libexec/path_helper" nil (clean-helper-buffer))

  (with-current-buffer (reuse-helper-buffer)
    (let*
      ((data (buffer-substring (point-min) (point-max)))
       (lines (split-string data "\n" t)))

      (mapcar 'path-to-list lines))) )

(defun setup-macos-paths ()
  "setup-macos-paths

   setup exec and man paths for macos
  "
  (interactive)

  (exec-path-from-shell-initialize)

  (let*
    ((helper-paths (macos-path-helper))

     (mac-exec-paths (car helper-paths))
     (mac-man-paths  (cadr helper-paths))

     (new-exec-path exec-path)
     (new-woman-path woman-path) )

;;    (message "helper-paths: %s" (pp helper-paths))
;;    (message "exec paths: %s" (pp mac-exec-paths))
;;    (message "man paths: %s" (pp mac-man-paths))

    (mapc
      (lambda (mac-path)
        (add-to-list 'new-exec-path mac-path t))
      mac-exec-paths)

    (mapc
      (lambda (man-path)
        (add-to-list 'new-woman-path man-path t))
      mac-man-paths)

    (when (file-directory-p user-brew)
      (map-paths user-brew bin-dirs 'new-exec-path)
      (map-paths user-brew man-dirs 'new-woman-path))

    (when (file-directory-p local-brew)
      (map-paths local-brew bin-dirs 'new-exec-path)
      (map-paths local-brew man-dirs 'new-woman-path))

    (when (file-directory-p opt-brew)
      (map-paths opt-brew bin-dirs 'new-exec-path)
      (map-paths opt-brew man-dirs 'new-woman-path))

    (maybe-add-path "/Applications/Emacs.app/Contents/MacOS/bin" 'new-exec-path)
    (maybe-add-path (concat (getenv "HOME") "/tools/local/libexec") 'new-exec-path)

    (setq
      exec-path (make-unique new-exec-path)
      woman-path (make-unique new-woman-path)) ))

(setup-macos-paths)

;; emacs gets trashed if there is no font specified through
;; the grail system. Im on big displays so crank up the font size.

(setq
  grail-font-family '("Cousine" "Hack" "Spleen" "DejaVu Sans Mono" "Courier New")
  grail-font-size 22)


