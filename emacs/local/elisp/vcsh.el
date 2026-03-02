;; -*- lexical-binding: t; -*-

(require 'subr-x)
(require 'vc)

(defun get-clean-cvsh-buffer ()
  (let
    ((py-buffer (get-buffer-create "*py.sh output*")))

    (with-current-buffer py-buffer
      (erase-buffer)
      py-buffer) ))

(defun get-cvsh-buffer ()
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

(defconst cvsh-commands (sort
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

(defconst cvsh-with-arguments
  '("global-virtual"
    "simple"
    "modadd"
    "modupdate"
    "track"
    "alpha"
    "beta"
    "start"))

(defconst cvsh-argument-prompts
  '(("global-virtual" . "NAME,PYTHON_VERSION")
    ("simple"    . "PKG")
    ("modadd"    . "REPO,BRANCH,LOCALDIR")
    ("modupdate" . "MODULE")
    ("track"     . "REMOTE,BRANCH")
    ("alpha"      . "FEAT,MSG")
    ("beta"      . "FEAT,MSG")
    ("start"     . "VERSION") ))

(defun cvsh-args-for (command)
  (let
    ((prompt (assoc command cvsh-argument-prompts)))

    (if prompt
      (cdr prompt)
      nil) ))

(defun cvsh-repo-dir ()
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
          (message "cvsh: could not find a VC directory. exiting.")) )) ))


(defun cvsh-args (command-name)
  (if (member command-name cvsh-with-arguments)
    (let
      ((args (read-from-minibuffer (concat "cvsh args [" (cvsh-args-for command-name) "]: "))))

      (if args
        (cons command-name (split-string args))
        (error (concat "cvsh: no args given for command with args: " command-name))) )
    (list command-name)) )

(defun cvsh-quit ()
  (interactive)

  (other-window 1)
  (delete-other-windows)

  (kill-buffer (get-cvsh-buffer)) )

(defun cvsh ()
  (interactive)
  (let
    ((command (helm
                :sources (helm-build-sync-source
                           "commands"
                           :candidates cvsh-commands
                           :fuzzy-match t)
                :preselect "info"
                :buffer "py.sh commands")))

    (let*
      ((default-directory (cvsh-repo-dir))
        (status (apply 'call-process
                  "py.sh"                             ;; program
                  nil                                 ;; infile
                  (get-clean-cvsh-buffer)             ;; output buffer
                  nil                                 ;; don't display
                  (cvsh-args command)) ))             ;; cvsh command and sometimes args

      (if (equal status 0)
        (progn
          (with-current-buffer (get-cvsh-buffer)
            (keymap-local-set "q" 'cvsh-quit))

          (pop-to-buffer (get-cvsh-buffer)) )
        (message "py.sh failed with: %d" status)) ) ))

(provide 'vcsh)
