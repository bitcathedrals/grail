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

(defconst py-commands '("tools-unix"
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

                        "purge"))

(defun pysh (command)
  (interactive (list (completing-read
                       "py.sh: "           ;; prompt
                       delta-conventional  ;; completions
                       nil                 ;; predicate
                       t                   ;; require match
                       nil                 ;; initial input
                       nil                 ;; history
                       "info"              ;; default value
                       nil)))              ;; inherit input method
  (let*
    ((default-directory (vc-root-dir))
     (status (call-process
               (concat default-directory "py.sh") ;; program
               nil                                ;; infile
               (get-clean-pysh-buffer)            ;; output buffer
               nil                                ;; don't display
               command)))                         ;; pysh command

    (if (equal status 0)
      (pop-to-buffer (get-pysh-buffer))
      (message "py.sh failed with: %d" status)) ))
