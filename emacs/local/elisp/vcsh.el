;; -*- lexical-binding: t; -*-

(require 'subr-x)
(require 'vc)

(defun get-clean-cvsh-buffer ()
  (let
    ((py-buffer (get-buffer-create "*vc output*")))

    (with-current-buffer py-buffer
      (erase-buffer)
      py-buffer) ))

(defun get-cvsh-buffer ()
  (get-buffer-create "*vc output*"))

(defconst cvsh-commands (sort
                          '("tools-zshrc"
                            "tools-custom"
                            "tools-prompt"

                            "tools-brew-init"
                            "tools-brew-upgrade"
                            "tools-brew-install"
                            "tools-brew-rebuild"

                            "dependencies-init"
                            "dependencies-upgrade"
                            "dependencies-install"
                            "dependencies-rebuild"

                            "modadd"
                            "modinit"
                            "modpull"
                            "modrm"

                            "begin"
                            "end"
                            "bug"
                            "close"

                            "goto"
                            "beta"
                            "tag"
                            "diff"

                            "report"
                            "status"

                            "goto"

                            "verify"

                            "commit"
                            "show"
                            "get"
                            "rebase"
                            "patch"

                            "pending"
                            "list"

                            "history"
                            "ahead"
                            "behind"

                            "up"
                            "down"
                            "merge"
                            "integrate"
                            "publish"
                            "cat"
                            "rb"

                            "check"
                            "start"
                            "release"

                            "help"
                            "man")))

(defconst cvsh-argument-prompts
  '(("commit"    . "LINE, LINE")
    ("tools-brew-install" . "PACKAGES")
    ("dependencies-install" . "PACKAGES")

    ("bug" . "NAME")
    ("begin" . "NAME")

    ("bug" . "NAME")
    ("begin" . "NAME")

    ("goto" . "BRANCH")

    ("beta" . "DESCRIPTION")
    ("tag" . "VERSION, DESCRIPTION")

    ("diff" . "OLDER_RANGE, NEWER_RANGE")
    ("TAG" . "VERSION, DESCRIPTION")
       ))

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

(defun vcsh ()
  (interactive)
  (let
    ((command (helm
                :sources (helm-build-sync-source
                           "commands"
                           :candidates cvsh-commands
                           :fuzzy-match t)
                :preselect "info"
                :buffer "vc commands")))

    (let*
      ((default-directory (cvsh-repo-dir))
        (status (apply 'call-process
                  "vc"                             ;; program
                  nil                                 ;; infile
                  (get-clean-cvsh-buffer)             ;; output buffer
                  nil                                 ;; don't display
                  (cvsh-args command)) ))             ;; cvsh command and sometimes args

      (if (equal status 0)
        (progn
          (with-current-buffer (get-cvsh-buffer)
            (keymap-local-set "q" 'cvsh-quit))

          (pop-to-buffer (get-cvsh-buffer)) )
        (message "vc failed with: %d" status)) ) ))

(provide 'vcsh)
