;; -*-no-byte-compile: t; -*-

(require 'custom-key)
(require 'utilities)

;;
;; some generic code editing stuff
;;

(defun toggle-comment-region ()
  "toggle-comment-region

   comment or uncomment the region
  "
  (interactive)
  (comment-or-uncomment-region (mark) (point)) )

(defun toggle-comment-buffer ()
  "toggle-comment-buffer

   comment or uncomment the region
  "
  (interactive)

  (mark-whole-buffer)
  (call-interactively 'toggle-comment-region) )

(defun programming-search-missing ()
  (interactive)
  (message "function signature to find is not specified"))

(defvar configure-programming-hook nil
  "hook so other programming tools can run after programming-mode-generic")

(defun programming-mode-generic ( &optional fn-search )
  "Enable my programming customizations for the buffer"

  ;; whitespace
  (setq indent-tabs-mode nil)
  (whitespace-mode)

  ;; run hooks for programming configuration
  (run-custom-hooks configure-programming-hook)

  ;; better return key for programming
  (local-set-key (kbd "<return>") 'newline-and-indent)

  (add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p t)
  (ruler-mode))

(defun get-clean-report-buffer ()
  (let
    ((report-buffer (get-buffer-create "*report output*")))

    (with-current-buffer report-buffer
      (erase-buffer)
      report-buffer) ))

(defun get-report-buffer ()
  (get-buffer-create "*report output*"))

(defconst conventional-list '("feat" "fix" "bug" "issue" "sync" "merge" "alpha" "beta"))

(defun insert-sync (module)
  (interactive "senter syncd module: ")
  (insert (concat "(sync) [" (iso8601-string) "] syncd: " module)) )

(defun insert-report (type)
  (interactive (list (completing-read
                       "commit type|fix: " ;; prompt
                       conventional-list ;; completions
                       nil ;; predicate
                       t ;; require match
                       nil ;; initial input
                       nil ;; history
                       "fix" ;; default value
                       nil))) ;; inherit input method
  (let*
    ((initial (if (string-equal type "sync")
                (concat "(sync) [" (iso8601-string) "] ")
                (concat "(" type ")")))
     (message (read-from-minibuffer
                "message: " ;; prompt
                (concat initial ": ")  ;; initial value
                nil ;; keymap
                nil  ;; read - dont eval as lisp
                nil  ;; history
                "(fix)" ;; default value
                nil)) ;; inherit input method ? nope don't care.
      (default-directory (vc-root-dir)) )

    (let
      ((report-type (if (yes-or-no-p "release? yes = release|no = status ")
                      "release-report"
                      "status-report")))
      (call-process
        (concat (vc-root-dir) "/py.sh") ;; program
        nil ;; infile
        (get-clean-report-buffer) ;; output buffer
        nil ;; don't display
        report-type)) ;; report command

    (if (yes-or-no-p "view? yes = view|no = insert")
      (pop-to-buffer (get-report-buffer))
      (let
        ((report-contents (with-current-buffer (get-report-buffer)
                            (buffer-substring (point-min) (point-max))) ))
        (if (string-empty-p report-contents)
          (message "insert-report: no report output")
          (insert (concat type " " message "\n" report-contents))) )) ))

(provide 'programming-generic)
