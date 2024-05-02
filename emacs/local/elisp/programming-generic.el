;; -*- lexical-binding: t; no-byte-compile: t; -*-

(require 'custom-key)
(require 'utilities)
(require 'subr-x)

(require 'pysh)

(require 'eglot)

(require 'puni)

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

(defun search-buffer-functions ()
  "search-buffer-functions

   search the buffer for functions
  "
  (interactive)

  (if (boundp 'programming-generic/buffer-functions)
    (funcall programming-generic/buffer-functions)
    (message "no programming-generic/buffer-functions defined in buffer.")) )


(defun programming-mode-generic ( &optional fn-search )
  "Enable my programming customizations for the buffer"

  ;; whitespace
  (setq indent-tabs-mode nil)
  (whitespace-mode)

  ;; run hooks for programming configuration
  (run-custom-hooks configure-programming-hook)

  ;; better return key for programming
  (keymap-local-set "<return>" 'newline-and-indent)

  (add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p t)

  ;; turn on flyspell
  (flyspell-prog-mode)

  (ruler-mode)

  (display-line-numbers-mode)

  (keymap-local-set "C-c <right>" 'puni-forward-sexp)
  (keymap-local-set "C-c <left>"  'puni-backward-sexp)
  (keymap-local-set "C-c m"       'puni-expand-region)
  (keymap-local-set "C-c <up>"    'puni-backward-sexp-or-up-list)
  (keymap-local-set "C-c <down>"  'down-list)

  (when fn-search
    (set (make-local-variable 'programming-generic/buffer-functions) fn-search))

  (custom-key-group "puni" "p"  nil
    ("<right>" . puni-forward-kill-word)
    ("<left>"  . puni-backward-kill-word)
    ("<up>"    . puni-kill-line)
    ("<down>"  . puni-backward-kill-line)
    ("b"       . puni-beginning-of-sexp)
    ("e"       . puni-end-of-sexp) )

  (custom-key-group "coding" "x"  nil
    ("c" . toggle-comment-region)
    ("f" . xref-find-definitions)
    ("a" . xref-find-apropos)
    ("w" . xref-find-definitions-other-window)
    ("p" . xref-go-back)
    ("n" . xref-go-forward)) )

(defun get-clean-report-buffer ()
  (let
    ((report-buffer (get-buffer-create "*report output*")))

    (with-current-buffer report-buffer
      (erase-buffer)
      report-buffer) ))

(defun get-report-buffer ()
  (get-buffer-create "*report output*"))

(defconst delta-conventional '("feat" "fix" "bug" "issue" "sync" "merge" "alpha" "beta" "release" "refactor" "doc"))

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

   insert a conventional commit.
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

(defun setup-magit-for-delta ()
  "setup-magit-for-delta

   setup delta-insert keybindings for magit mode
  "
  (interactive)

  (keymap-local-set "C-c i" 'delta-insert))

(add-hook 'magit-mode-hook 'setup-magit-for-delta)

(provide 'programming-generic)
