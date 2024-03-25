;; -*- lexical-binding: t; no-byte-compile: t; -*-

(setq-default lexical-binding t)

(defun extract-string-with-regex (regexp)
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward regexp nil t)
      (match-string 0))))

(defun squeeze-slashes (path)
  (replace-regexp-in-string "//+" "/" path))

(defun maybe-add-path (path path-list)
  (let
    ((candidate-path (squeeze-slashes path)))

    (when (and (not (string-empty-p candidate-path))
               (file-directory-p candidate-path))
      (add-to-list path-list candidate-path)) ))

(defun map-paths ( dir sub-dirs path-list)
  "from a stem directory DIR look in SUB-DIR and add missing paths to PATH-LIST"
  (mapc
    (lambda (sub)
      (maybe-add-path (concat dir sub) path-list))
    sub-dirs))

(defun iso8601-string ()
  "iso8601-string

   return a iso8601-string"
  (concat
    (format-time-string "%Y-%m-%dT%T")
    ((lambda (x)
       (concat (substring x 0 3) ":" (substring x 3 5)))
      (format-time-string "%z"))) )
