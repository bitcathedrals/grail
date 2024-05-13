;; -*-no-byte-compile: t; -*-

(require 'subr-x)

(require 'puni)

(require 'pysh)
(require 'borg-repl)

(defconst python/mode-name "python")

(defconst python/virtualenv-name "*virtualenv*")

(defun python/virtualenv-output ()
  (with-current-buffer (get-buffer-create python/virtualenv-name)
    (erase-buffer)
    (current-buffer)) )

(defun python/virtualenv-list ()
  (with-current-buffer (python/virtualenv-output)
    (call-process "python-venv-list.sh" nil t nil)
    (split-string (buffer-substring-no-properties (point-min) (point-max)) nil t)) )

(python/virtualenv-list)

(defconst python/virtualenv-select-name "*python environments*")

(defun python/virtualenv-select-buffer ()
  (get-buffer-create python/virtualenv-select-name))

(defun python/virtualenv-select ()
  (interactive)
  (helm
    :sources (helm-build-sync-source
               "virtualenvs"
               :candidates (python/virtualenv-list)
               :fuzzy-match t)
    :preselect "tools"
    :buffer (python/virtualenv-select-buffer)) )

(defun python/environment-name (venv)
  (concat "*python:" venv "*"))

(defun python/environment-buffer (venv)
  (get-buffer-create (python/venv-name)) )

(defun python/environment-close (interpreter-buffer)
  (if (and interpreter-buffer (buffer-live-p interpreter-buffer))
    (progn
      (when window-visible-p interpreter-buffer
        (other-window 1)
        (delete-other-windows))

      (with-current-buffer interpreter-buffer
        (delete-process python-interpreter)
        (message "closed interpreter and buffer: %s" python-interpreter-venv) )

      (kill-buffer interpreter-buffer) )
    (message "python interpreter buffer already dead: %s" (buffer-name interpreter-buffer)) ))

(defun python/environment-active (buffer)
  (if (buffer-live-p buffer)
    (with-current-buffer repl-buffer
      (if (and
            (buffer-local-value python-interpreter buffer)
            (process-live-p (buffer-local-value python-interpreter)))
        t
        nil))
    nil) )

(defconst python/environment-select-name "*python: select environment*")

(defun python/environment-select-buffer ()
  (get-buffer-create (python/environment-select-name)) )

(defun python/environment-select ()
  (interactive)
  (let
    ((environments nil))

    (mapc
      (lambda (buffer)
        (let
          ((name (buffer-name buffer)))

          (when (string-match "^python:" name)
            (add-to-list 'environments name)) ))

      (buffer-list))

    (helm
      :sources (helm-build-sync-source
                 "environments"
                 :candidates environments
                 :fuzzy-match t)
      :buffer (python/environment-select-buffer)) ))

(defun python/interpreter-sentinel (interpreter status)
  (when (string-match "finished|deleted|exited|failed|broken" status)
    (python/interpreter-close (process-buffer interpreter)) ) )

(defun python/interpreter-create (venv buffer)
  (with-current-buffer buffer
    (let
      ((default-directory (pysh-repo-dir)))

      (erase-buffer)

      (setq-local
        python-environment venv
        python-interpreter (make-process
                             :name (python/venv-name venv)
                             :command "python-interpreter"
                             :buffer buffer
                             :connection-type 'pty'
                             :stderr nil
                             :sentinel 'python/interpreter-sentinel
                             venv
                             default-directory)) )
    t))

(defun python/environment-default ()
  (let
    ((env-buffer (buffer-local-value python/environment (current-buffer))))

    (if env-buffer
      (if (python/environment-active env-buffer)
        env-buffer
        (if (yes-or-no-p "existing environment? yes = select|no = create")
          (let
            ((env-chosen (call-interactively 'python/environment-select)))
            (setq-local python/environment (get-buffer env-chosen)) )
          (let*
            ((venv (python/virtualenv-select))
              (new-buffer (python/environment-buffer venv)))

            (interpreter-create venv new-buffer)
            new-buffer) ))
      (let*
        ((venv (python/virtualenv-select))
          (new-buffer (python/environment-buffer venv)))

        (interpreter-create venv new-buffer)

        (setq-local python/environment new-buffer)
        new-buffer) ) ))

(defun python/repl-get ()
  (with-current-buffer (python/environment-default)
    python-interpreter) )

(defun python/repl-string (code)
  (process-send-string (python/repl-get) code) )

(defun python/repl-sexp ()
  (interactive)

  (let
    ((stop  (point))
     (start (save-excursion
             (call-interactively 'puni-backwards-sexp)
             (point))))

    (python/repl-string (buffer-substring-no-properties start stop)) ))

(defun python/repl-name ()
  (interactive)
  (if (buffer-local-value python/environment)
    (buffer-name python/environment)
    "no python repl"))

(defun python/repl-pop ()
  (interactive)
  (pop-to-buffer (python/environment-default)) )

(defun python/repl-region ()
  (interactive)
  (let
    ((start (if (< (mark) (point)) (mark) (point)))
     (end (if (> (point) (mark)) (point) (mark))))

  (python/repl-string (buffer-substring-no-properties start end)) ))

(defun python/repl-buffer (code)
  (interactive)
  (python/repl-string (buffer-substring-no-properties (point-min) (point-max))) )

(defun python/setup-python-profile ()
  (interactive)
  (dwim-tab-localize-context (dwim-tab-make-expander 'dwim-tab/word-trigger 'python-completion-at-point)) )

(add-hook 'python-mode-hook 'python/setup-python-profile)

(provide 'profile/python)
