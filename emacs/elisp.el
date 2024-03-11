;; -*- lexical-binding: t; no-byte-compile: t; -*-

(defun extract-string-with-regex (regexp)
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward regexp nil t)
      (match-string 0))))

(defun squeeze-slashes (path)
  (replace-regexp-in-string "//+" "/" path))

(defun maybe-add-path (path path-list)
  (when (file-directory-p path)
    (add-to-list path-list (squeeze-slashes path))
    path-list))

(defun map-paths ( dir sub-dirs path-list)
  "from a stem directory DIR look in SUB-DIR and add missing paths to PATH-LIST"
  (mapc
    (lambda (sub)
        (let
          ((path (concat dir sub)))
          (maybe-add-path path path-list)))
    sub-dirs))

