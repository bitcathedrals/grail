;; -*-no-byte-compile: t; -*-

(defun recompile-elisp-all ()
  (interactive)

  (mapcar
    (lambda (dir)
      (when (file-directory-p dir)
        (message (concat "updating compiles in directory: " dir))

        (grail-ignore
          "recompile-elisp-all"
          (format "re-compiling directory: %s" dir)

          (byte-recompile-directory dir nil)) ))
    load-path))

(defun compile-elisp-all ()
  (interactive)

  (mapcar
    (lambda (dir)
      (when (file-directory-p dir)
        (message (concat "compiling in directory: " dir))

        (mapcar
          (lambda (file)
            (grail-ignore
              "compile-elisp-all"
              (format "compiling a elisp file: %s @ %s" file dir)

              (byte-compile-file file)) )
          (file-expand-wildcards (concat dir "/*.el")) )))
    load-path))

(defun compile-dist ()
  (interactive)

  (byte-recompile-directory grail-dist-git 0)
  (byte-recompile-directory grail-dist-elisp 0) )
