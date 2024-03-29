;; -*- lexical-binding: t; no-byte-compile: t; -*-

(require 'custom-key)
(require 'utilities)
(require 'subr-x)

(require 'pysh)

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

(defconst delta-conventional '("feat" "fix" "bug" "issue" "sync" "merge" "alpha" "beta" "release"))

(defun delta-sync-string (module)
  (interactive "senter syncd module: ")
  (concat "(sync) [" (iso8601-string) "] sync: " module))

(defun delta-staged-files ()
  (let
    ((file-list (mapcar
                  (lambda (staged-file)
                    (file-name-nondirectory staged-file))
                  (magit-staged-files)) ))

    (string-join file-list ",") ))


(defun delta-repo-dir ()
  (let
    ((repo-dir (vc-root-dir)))

    (if (stringp repo-dir)
      repo-dir
      (let*
        ((split (reverse (file-name-split default-directory))))

        (when (string-empty-p (car split))
          (setq split (cdr split)))

      (if (string-equal (car split) ".git")
        (concat (string-join (reverse (cdr split)) "/") "/")
        (progn
          (message "delta-repo-dir WARNING! could not find vc-root-dir, assuming \".\"")
          ".")) ) ) ))

(defun delta-status ()
  (interactive)
  (let*
    ((report-type (if (yes-or-no-p "release? yes = release|no = status ")
                    "release-report"
                    "status-report"))
      (default-directory (delta-repo-dir))
      (status (call-process
                (concat default-directory "py.sh") ;; program
                nil                                ;; infile
                (get-clean-report-buffer)          ;; output buffer
                nil                                ;; don't display
                report-type)))                     ;; report command

    (if (yes-or-no-p "view? yes = view|no = insert")
      (progn
        (pop-to-buffer (get-report-buffer))
        "")
      (with-current-buffer (get-report-buffer)
        (buffer-substring (point-min) (point-max))) ) ))

(defun delta-insert (type)
  "delta-insert

  "
  (interactive (list (completing-read
                       "commit type|fix: " ;; prompt
                       delta-conventional  ;; completions
                       nil     ;; predicate
                       t       ;; require match
                       nil     ;; initial input
                       nil     ;; history
                       "fix"   ;; default value
                       nil)))  ;; inherit input method
  (let
    ((content (if (string-equal type "sync")
                (call-interactively 'delta-sync-string)
                (let
                  ((message (read-from-minibuffer
                              "message: " ;; prompt
                              ""          ;; initial value
                              nil         ;; keymap
                              nil         ;; read - dont eval as lisp
                              nil         ;; history
                              "update"    ;; default value
                              nil)) )    ;; inherit input method ? nope don't care.

                  (if (string-equal type "release")
                    (concat "(release): " message "\n" (delta-status))
                    (concat "(" type "): " message "\n[" (delta-staged-files) "]") ) ))))
    (insert content)) )

(custom-key-group "code insert" "i"  t
  ("c" . comment-region)
  ("d" . delta-insert))

(provide 'programming-generic)
