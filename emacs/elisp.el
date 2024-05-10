;; -*- lexical-binding: t; no-byte-compile: t; -*-

(setq-default lexical-binding t)

(setq native-comp-async-report-warnings-errors 'silent)

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
      (add-to-list path-list candidate-path t)) ))

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

(defun minor-mode-active-list ()
  "minor-mode-active-list

   Get a list of which minor modes are enabled in the current buffer.
  "
  (let
    ((active nil))

    (mapc
      (lambda (minor-mode)
        (condition-case nil
          (when (and (symbolp minor-mode) (symbol-value minor-mode))
            (setq active (cons minor-mode active)))
          (error nil)) )
      minor-mode-list)

    (sort active 'string<) ))

(defun make-unique (list)
  (let
    ((seen-table (make-hash-table))
     (output nil))

    (mapc
      (lambda (element)
        (when (not (gethash element seen-table))
          (add-to-list 'output element t)) )
      list) ))

(defun print-unique-exec-path ()
  (interactive)

  (mapc
    (lambda (path)
      (princ (concat path "\n")))
    (make-unique exec-path)) )

(defun message-macos-paths ()
  (interactive)
  (mapc
    (lambda (path)
      (message "exec: %s" path))
    exec-path)

  (mapc
    (lambda (path)
      (message "woman: %s" path))
    woman-path))

