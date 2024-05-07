;; -*- lexical-binding: t; no-byte-compile: t; -*-

(require 'subr-x)

(defun get-clean-pysh-buffer ()
  (let
    ((py-buffer (get-buffer-create "*py.sh output*")))

    (with-current-buffer py-buffer
      (erase-buffer)
      py-buffer) ))

(defun get-pysh-buffer ()
  (get-buffer-create "*py.sh output*"))

;; repl    = execute ptpython in pyenv
;; global-virtual
;; simple     = <pkg> do a simple pyenv pip install without pipenv

;; modadd <1> <2> <3>  = add a submodule where 1=repo 2=branch 3=localDir (commit after)
;; modupdate <module>  = pull the latest version of the module
;; modrm  <submodule>  = delete a submodule

;; track <1> <2>  = set upstream tracking 1=remote 2=branch
;; tag-alpha  <feat> <msg> = create an alpha tag with the feature branch name and message
;; tag-beta   <feat> <msg> = create a beta tag with the devel branch feature and message

;; start      = initiate an EDITOR session to update VERSION in python.sh, reload config,

(defconst pysh-commands (sort
                          '("tools-unix"
                             "tools-zshrc"
                             "tools-custom"
                             "tools-prompt"

                             "python-versions"
                             "project-virtual"
                             "global-virtual"
                             "virtual-destroy"
                             "project-destroy"
                             "global-destroy"
                             "virtual-list"
                             "virtual-current"

                             "minimal"
                             "bootstrap"
                             "pipfile"
                             "project"
                             "show-paths"
                             "add-paths"
                             "rm-paths"
                             "site"
                             "test"
                             "python"
                             "run"

                             "versions"
                             "locked"
                             "all"
                             "update"
                             "remove"
                             "list"

                             "build"
                             "buildset"
                             "mkrelease"
                             "runner"

                             "modinit"
                             "modall"

                             "info"
                             "verify"
                             "status"
                             "fetch"
                             "pull"
                             "staged"
                             "merges"
                             "releases"
                             "history"
                             "summary"
                             "delta"
                             "ahead"
                             "behind"
                             "release-report"
                             "status-report"

                             "graph"
                             "upstream"
                             "sync"
                             "check"

                             "release"
                             "upload"

                             ;; interactive commands

                             "global-virtual"
                             "simple"
                             "modadd"
                             "modupdate"

                             "track"
                             "alpha"
                             "beta"

                             "start")
                          'string-lessp))

(defconst pysh-with-arguments
  '("global-virtual"
    "simple"
    "modadd"
    "modupdate"
    "track"
    "alpha"
    "beta"
    "start"))

(defconst pysh-argument-prompts
  '(("global-virtual" . "NAME,PYTHON_VERSION")
    ("simple"    . "PKG")
    ("modadd"    . "REPO,BRANCH,LOCALDIR")
    ("modupdate" . "MODULE")
    ("track"     . "REMOTE,BRANCH")
    ("alpha"      . "FEAT,MSG")
    ("beta"      . "FEAT,MSG")
    ("start"     . "VERSION") ))

(defun pysh-args-for (command)
  (let
    ((prompt (assoc command pysh-argument-prompts)))

    (if prompt
      (cdr prompt)
      nil) ))

(defun pysh-repo-dir ()
  (let
    ((directory (vc-root-dir)))

    (if directory
      directory
      (let
        ((found (call-interactively 'helm-find-files)))

        (if found
          (with-current-buffer found
            (if (equal major-mode 'dired-mode)
              (dired-current-directory)
              buffer-file-name))
          (message "pysh: could not find a VC directory. exiting.")) )) ))


(defun pysh-args (command-name)
  (if (member command-name pysh-with-arguments)
    (let
      ((args (read-from-minibuffer (concat "pysh args [" (pysh-args-for command-name) "]: "))))

      (if args
        (cons command-name (split-string args))
        (error (concat "pysh: no args given for command with args: " command-name))) )
    (list command-name)) )

(defun pysh-quit ()
  (interactive)

  (other-window 1)
  (delete-other-windows)

  (kill-buffer (get-pysh-buffer)) )

(defun pysh ()
  (interactive)
  (let
    ((command (helm
                :sources (helm-build-sync-source
                           "commands"
                           :candidates pysh-commands
                           :fuzzy-match t)
                :preselect "info"
                :buffer "py.sh commands")))

    (let*
      ((default-directory (pysh-repo-dir))
        (status (apply 'call-process
                  (concat default-directory "py.sh") ;; program
                  nil                                ;; infile
                  (get-clean-pysh-buffer)            ;; output buffer
                  nil                                ;; don't display
                  (pysh-args command)) ))            ;; pysh command and sometimes args

      (if (equal status 0)
        (progn
          (with-current-buffer (get-pysh-buffer)
            (keymap-local-set "q" 'pysh-quit))

          (pop-to-buffer (get-pysh-buffer)) )
        (message "py.sh failed with: %d" status)) ) ))

(provide 'pysh)
