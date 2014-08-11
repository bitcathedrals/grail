;;;----------------------------------------------------------------------
;; grail-profile.el
;;----------------------------------------------------------------------
(require 'cl)
(require 'async-exec)
(require 'sync-exec)

(defun quote-string-for-shell ( string )
  "quote-string-for-shell STRING

   quote the string with ' for the shell.
  "
  (concat "\'" string "\'"))

(defvar grail-masked-profiles
  nil
  "List of grail profiles masked by the user.")

(defvar grail-requested-profiles
  nil
  "List of grail profiles requested by the user.")

(defvar grail-failed-profiles
  nil
  "List of grail profiles that failed to load.")

(defvar grail-loaded-profiles
  nil
  "List of grail profiles that have loaded.")

(defun grail-load-requested-profiles ()
  "grail-load-requested-profiles

   Load the profiles in the request list grail-requested-profiles.
  "
  (when grail-requested-profiles
    (let
      ((order-sorted (sort grail-requested-profiles (lambda ( a b )
                                                      (when (< (car a) (car b)) t)
                                                      )) ))
      (setq grail-requested-profiles nil)

      (mapc
        (lambda ( profile-order )
          (message "grail: loading order %d -> %s" (car profile-order) (cdr profile-order))
          (mapc
            (lambda ( profile )
              (catch 'skip-profile
                (when (member profile grail-masked-profiles)
                  (message "ignoring masked profile %s" profile)
                  (throw 'skip-profile t))

                (message "grail: loading profile %s" (concat grail-local-profiles profile))

                (let
                  ((trapped (catch 'grail-trap
                              (catch 'grail-disabled
                                (grail-try-elisp (concat grail-local-profiles profile)) ))))

                  (if (consp trapped)
                    (progn
                      (push (cons (car profile-order) profile) grail-failed-profiles)
                      (message "grail: profile %s failed to load" profile)
                      (apply 'grail-report-errors
                        (format "grail: profile %s failed to load" profile) trapped))
                    (push profile grail-loaded-profiles)) )))
            (cdr profile-order)))
          order-sorted)
      t)))

(defun grail-retry-failed-profiles ()
  "grail-retry-failed-profiles

   Retry the loading of any profiles that previously failed to load.
  "
  (interactive)

  (setq grail-requested-profiles grail-failed-profiles)
  (setq grail-failed-profiles nil)
  (grail-load-requested-profiles))

(defun use-grail-profiles ( order &rest request-list )
  "use-grail-groups: ORDER LIST

   request a list of string quoted groups to be loaded after the configuration
   files have been loaded.
  "
  (push (cons order request-list) grail-requested-profiles))


(defun mask-grail-profiles ( &rest request-list )
  "use-grail-groups: ORDER LIST

   mask profiles to not be loaded.
  "
  (setq grail-masked-profiles (append request-list grail-masked-profiles)))

;;----------------------------------------------------------------------
;; installation support routines.
;;----------------------------------------------------------------------

(defvar grail-save-downloads nil
  "when t downloaded archive files will be saved to grail-dist-dir")

(defun grail-recursive-delete-directory ( path )
  "grail-recursive-delete-directory PATH

   recursively delete the directory PATH. t on success.
  "
  (grail-fail
    "grail-recursive-delete-directory"
    "deleting a directory"

    (unless (equal 0 (call-process-shell-command "rm" nil nil nil "-r" path))
      (grail-signal-fail "grail-recursive-delete-directory"
        (format "path %s is not a directory or the user does not have permissions" path)) ) ))

(defvar grail-dist-default-directory grail-dist-elisp)

(defun grail-dist-default-to-packages ()
  (setq grail-dist-default-directory grail-dist-elisp))

(defun grail-dist-default-to-docs ()
  (setq grail-dist-default-directory grail-dist-docs dir-name))

(defun grail-dist-install-directory ( &optional package )
  "grail-dist-install-directory &optional string:PACKAGE

   Ensure that the installation directory exists. The default is grail-dist-elisp,
   however for multi-file packages an optional package name can be supplied.

   The path of the installation directory is returned for the installer's use.
  "
  (grail-dir-always (expand-file-name
                             (concat
                             (if package
                               (concat grail-dist-default-directory "/" package)
                               grail-dist-default-directory)
                               "/"))))

(defun grail-download-dir-and-file-path ( name )
  (let
    ((dl-dir  nil))

    (when (condition-case trapped-error
          (progn
            (setq dl-dir (if grail-save-downloads
                            grail-dist-dir
                            (make-temp-file "grail" t)))
            t)
          (error
              (throw 'grail-trap
                '((message "grail: grail-download-dir-and-file-path could not create a download path for %s" name))) ))
      (cons dl-dir (expand-file-name (concat dl-dir "/" name))) )))

(defun grail-cleanup-download ( dl-dir-and-file &optional ignore-save )
  "grail-cleanup-download

   delete the directory and the downloaded files.

   TODO: save downloads option.
  "
  (when dl-dir-and-file
    (if grail-save-downloads
      ;; when grail-save-downloads is enabled absolutely do not recursive delete !
      (when (not ignore-save)
        (delete-file (cdr dl-dir-and-file)))
      ;; otherwise it is a temp dir so nuke it
      (grail-recursive-delete-directory (car dl-dir-and-file))) ))

;;----------------------------------------------------------------------
;; async chain.
;;----------------------------------------------------------------------

(defun grail-buffer-exists-p ( buffer-name )
  (if (not (eq nil (get-buffer buffer-name)))
    t
    nil))

(defun get-time-in-seconds ()
  (truncate (time-to-seconds (current-time))))

(defun grail-process-async-wait ( proc-buffer-name )
  (let
    ((timeout 30)
     (last-size (buffer-size (get-buffer proc-buffer-name)))
     (last-time (get-time-in-seconds)))

  (catch 'timeout
    (while (grail-buffer-exists-p proc-buffer-name)
      (sleep-for 0.25)

      (let
        ((check-size (buffer-size (get-buffer proc-buffer-name))))

        (if (> check-size last-size)
          (progn
            (setq last-size check-size)
            (setq last-time (get-time-in-seconds)))
          (when (>  (- (get-time-in-seconds) last-time) timeout)
            (throw 'timeout nil)) )))
    t)))


;;----------------------------------------------------------------------
;; grail-run-and-wait
;;----------------------------------------------------------------------

(defun grail-run-and-wait ( base-name fn )
  (let*
    ((output-buffer-name   (generate-new-buffer-name base-name))
     (output-buffer-object (pop-to-buffer output-buffer-name nil t))
     (status nil))

    (funcall fn output-buffer-object)

    (grail-process-async-wait output-buffer-name) ))

;;----------------------------------------------------------------------
;; installation library
;;----------------------------------------------------------------------

(defun grail-file-url ( name url &optional path )
  "grail-file-url NAME URL &optional PATH

   install from URL into PATH with name NAME.  nil is returned
   when successful, otherwise an error is thrown.
  "
  (let
    ((install-path (concat (grail-dist-install-directory path) name)))

    (with-temp-buffer
      (url-insert-file-contents url)
      (let
        ((buffer-file-coding-system 'no-conversion))
        (write-file install-path)))

    (grail-report-info "grail-load file-installer" "completed install for" url path) ))

(defun grail-wget-url-async ( url path output-buffer )
  "grail-wget-url-async URL PATH OUTPUT-BUFFER

   retrieve the URL to PATH, with OUTPUT-BUFFER as the output
   buffer. The process object created is returned, or nil if a
   process could not be created.
  "
  (condition-case trapped-error
    (start-process-shell-command "grail-wget" output-buffer
      "wget"
      "--progress=dot:binary"
      (quote-string-for-shell url) "-O" (quote-string-for-shell path))
    (error
      (progn
        (message "grail-wget-url failed %s" (format-signal-trap trapped-error))
        nil)) ))

;;
;; tar
;;

(defvar grail-untar-strip-command nil)

(defun grail-untar-strip-by-el ()
  (setq grail-untar-strip-command (concat "--wildcards" " " (quote-string-for-shell "*.el"))) )

(defun grail-untar-strip-by-depth ( levels )
  (setq grail-untar-strip-command
    (if (> levels 0)
      (concat "--strip-components" " " (number-to-string levels))
      " ")))

(defun grail-untar-async ( path target-dir compression output-buffer )
  "grail-untar-async PATH DIR COMPRESSION OUTPUT-BUFFER

   untar PATH in DIR with output going to OUTPUT-BUFFER.
   return the process object or nil if there was an error.
  "
  (condition-case trapped-error
    (start-process-shell-command "grail-untar" output-buffer
      "tar"
      (concat
        "xv"
        (cond
          ((equal "gz"  compression) "z")
          ((equal "bz2" compression) "j")
          (t (signal error (format "grail: error! unsupported compression %s" compression))))
        "f")
      (quote-string-for-shell path)
      "-C" (quote-string-for-shell target-dir)
      grail-untar-strip-command)
    (error
      (progn
        (message "grail-untar-async failed %s" (format-signal-trap trapped-error))
        nil)) ))

(defun grail-untar-local-archive ( path compression tar-buffer )
  "grail-untar-local-archive PATH COMPRESSION

   extract the local archive PATH in directory name with COMPRESSION.
  "
  (lexical-let
    ((archive-path   path)
     (grail-buffer   tar-buffer))

    (async-exec-chain
      ;; start the untar
      (lambda ()
        (grail-untar-async archive-path grail-dist-elisp compression grail-buffer))

      ;; if it doesn't start
      (lambda ()
        (message "archive program did not start for %s!" archive-path))

      ;; FIXME: how do we clean up the target directory ?
      (lambda ( exit-status )
        (message "extracting %s failed! status %s " archive-path exit-status))

      ;; what to do when it finishes.
      (lambda ()
        (message "extracting %s has completed." archive-path))

      ;; no chaining
      nil) ))

(defun grail-untar-remote-archive ( name url compression tar-buffer)
  "grail-untar-remote-archive NAME URL COMPRESSION

   Download a tarball from a remote url and install it. It is currently
   hard-coded for tar, but that could be changed fairly easily.
  "
  (save-excursion
    (lexical-let*
      ((target-dir      name)
       (dl-dir-and-file nil)
       (old-window      (selected-window))
       (install-buffer  tar-buffer))

      (catch 'abort
        ;; confirm with the user that they want to install the file.
        (unless (yes-or-no-p (format "download and install %s? " name))
          (throw 'abort nil))

        ;; signal the start of the download in the grail buffer.
        (insert (format "Starting the download of %s\n" url))

        ;; create a temporary directory to download into
        (unless (setq dl-dir-and-file (grail-download-dir-and-file-path (concat name ".tar." compression)))
          (throw 'abort "could not create a temporary directory for the download"))

        (lexical-let
          ((dl-url  url)
           (compression-type compression))

          (async-exec-chain
            ;; start the download with wget
            (lambda ()
              (grail-wget-url-async
                dl-url
                (cdr dl-dir-and-file)
                install-buffer))

            ;; the downloader doesn't start cleanup function
            (lambda ()
              (insert "could not start the download! Install aborted.\n")
              (grail-cleanup-download dl-dir-and-file t))

            ;; the downloader fail cleanup function
            (lambda ( exit-status )
              (grail-cleanup-download dl-dir-and-file t)
              (message "download of %s failed! Install aborted, and downloads deleted." (cdr dl-dir-and-file)))

            ;; the downloader succeeded function
            (lambda ()
              (insert "grail: download completed\n")
              t)

            ;; the chain function
            (lambda ()
              (async-exec-chain
                ;; start the untar
                (lambda ()
                  (message "starting the untar")
                  (grail-untar-async (cdr dl-dir-and-file) (grail-dist-install-directory target-dir)
                    compression-type install-buffer))

                ;; tar doesn't start cleanup function
                (lambda ()
                  (insert "could not start tar to extract the downloaded archive. Install aborted, deleting downgrail: cleaning up downloads
loads.\n")
                  (grail-cleanup-download dl-dir-and-file t))

                ;; the tar fail cleanup function
                (lambda ( exit-status )
                  (insert (format "could not install files in %s from downloaded archive." grail-dist-elisp))
                  (grail-cleanup-download dl-dir-and-file t))

                ;; the tar succeeded function
                (lambda ()
                  (insert "grail: cleaning up downloads\n")
                  (grail-cleanup-download dl-dir-and-file)
                  (kill-buffer install-buffer)
                  t)

                ;; terminate the chain.
                nil))) )
        ;; return nil if an abort is not thrown.
        nil)) ))

;;----------------------------------------------------------------------
;; tar installer
;;
;;----------------------------------------------------------------------

(defvar grail-tar-buffer "*grail tar*")

(defun grail-tar-local-installer ( name url compression )
  (let
    ((name-arg        name)
     (url-arg         url)
     (compression-arg compression))

    (grail-run-and-wait grail-tar-buffer
      (lambda ( run-buffer )
        (grail-untar-remote-archive name-arg url-arg compression run-buffer))) ))

(defun grail-tar-remote-installer ( name url compression )
  (let
    ((name-arg        name)
     (url-arg         url)
     (compression-arg compression))

    (grail-run-and-wait grail-tar-buffer
      (lambda ( run-buffer )
        (grail-untar-remote-archive name-arg url-arg compression run-buffer))) ))

;;----------------------------------------------------------------------
;; version control
;;
;; The preferred way to integrate third party packages: a version control
;; checkout.
;;----------------------------------------------------------------------

;;
;; cvs
;;

(defun grail-cvs-async ( url dir module output-buffer )
  "grail-cvs-async URL PATH OUTPUT-BUFFER

   retrieve a remote tree via cvs.
  "
  (condition-case trapped-error
    (let
      ((default-directory (grail-garuntee-dir-path dir)))

      (start-process-shell-command "grail-cvs" output-buffer
        "cvs"
        "-d"
        (quote-string-for-shell url)
        "co"
        (quote-string-for-shell module)))
    (error
      (progn
        (message "grail-cvs-async failed %s" (format-signal-trap trapped-error))
        nil)) ))

(defconst grail-cvs-buffer "*grail-cvs*")

(defun grail-cvs-installer ( module url )
  (lexical-let
    ((module-arg module)
     (url-arg    url))

    (grail-run-and-wait grail-cvs-buffer
      (lambda ( run-buffer )
        (grail-cvs-async url-arg grail-dist-cvs module-arg run-buffer))) ))

(defun grail-cvs-docs ( module url )
  (lexical-let
    ((module-arg module)
     (url-arg    url))

    (grail-run-and-wait grail-cvs-buffer
      (lambda ( run-buffer )
        (grail-cvs-async url-arg grail-dist-docs module-arg run-buffer))) ))

;;
;; git
;;

(defun grail-git-async ( url dir module output-buffer )
  "grail-git-async URL PATH OUTPUT-BUFFER

   retrieve the URL to PATH, with OUTPUT-BUFFER as the output
   buffer. The process object created is returned, or nil if a
   process could not be created.
  "
  (condition-case trapped-error
    (let
      ((default-directory (grail-garuntee-dir-path dir)))

      (start-process-shell-command "grail-git" output-buffer
        "git"
        "clone"
        (quote-string-for-shell url)
        (quote-string-for-shell module)))
    (error
      (progn
        (message "grail-git-async failed %s" (format-signal-trap trapped-error))
        nil)) ))

(defconst grail-git-buffer "*grail-git*")

(defun grail-git-installer ( module url )
  (lexical-let
    ((module-arg module)
     (url-arg    url))

    (grail-run-and-wait grail-git-buffer
      (lambda ( run-buffer )
        (grail-git-async url-arg grail-dist-git module-arg run-buffer))) ))

(defun grail-git-docs ( module url )
  (lexical-let
    ((module-arg module)
     (url-arg    url))

    (grail-run-and-wait grail-git-buffer
      (lambda ( run-buffer )
        (grail-git-async url-arg grail-dist-docs module-arg run-buffer))) ))

(defun grail-git-templates ( template-dir module url )
  (lexical-let
    ((module-arg module)
     (url-arg    url))

    (grail-run-and-wait grail-git-buffer
      (lambda ( run-buffer )
        (grail-git-async url-arg template-dir module-arg run-buffer))) ))

;;
;; svn
;;

(defun grail-svn-async ( url dir module output-buffer )
  "grail-svn-async URL PATH OUTPUT-BUFFER

   retrieve the URL to PATH, with OUTPUT-BUFFER as the output
   buffer. The process object created is returned, or nil if a
   process could not be created.
  "
  (condition-case trapped-error
    (let
      ((default-directory (grail-garuntee-dir-path dir)))

      (start-process-shell-command "grail-svn" output-buffer
        "svn"
        "checkout"
        (quote-string-for-shell url)
        (quote-string-for-shell module)))
    (error
      (progn
        (message "grail-svn-async failed %s" (format-signal-trap trapped-error))
        nil)) ))

(defconst grail-svn-buffer "*grail-svn*")

(defun grail-svn-installer ( module url )
  (lexical-let
    ((module-arg   module)
     (url-arg      url))

    (grail-run-and-wait grail-svn-buffer
      (lambda ( run-buffer )
        (grail-svn-async url-arg grail-dist-svn module-arg run-buffer))) ))

(defun grail-svn-docs ( module url )
  (lexical-let
    ((module-arg  module)
     (url-arg     url))

    (grail-run-and-wait grail-svn-buffer
      (lambda ( run-buffer )
        (grail-svn-async url-arg grail-dist-docs module-arg run-buffer))) ))

;;
;; bzr
;;

(defconst grail-bzr-buffer "*grail-bzr*")

(defun grail-bzr-async ( url dir module output-buffer )
  "grail-bzr-async URL PATH OUTPUT-BUFFER

   retrieve the URL to PATH, with OUTPUT-BUFFER as the output
   buffer. The process object created is returned, or nil if a
   process could not be created.
  "
  (condition-case trapped-error
    (let
      ((default-directory (grail-garuntee-dir-path dir)))

      (start-process-shell-command "grail-bzr" output-buffer
        "bzr"
        "branch"
        (quote-string-for-shell url)
        (quote-string-for-shell module)))
    (error
      (progn
        (message "grail-bzr-async failed %s" (format-signal-trap trapped-error))
        nil)) ))

(defun grail-bzr-installer ( module url )
  (lexical-let
    ((module-arg  module)
     (url-arg     url))

    (grail-run-and-wait grail-hg-buffer
      (lambda ( run-buffer )
        (grail-bzr-async url-arg grail-dist-bzr module-arg run-buffer))) ))

(defun grail-bzr-docs ( module url )
  (lexical-let
    ((module-arg  module)
     (url-arg     url))

    (grail-run-and-wait grail-hg-buffer
      (lambda ( run-buffer )
        (grail-bzr-async url-arg grail-dist-docs module-arg run-buffer))) ))

;;
;; mercurial
;;

(defconst grail-hg-buffer "*grail-hg*")

(defun grail-hg-async ( url dir module output-buffer )
  "grail-hg-async URL PATH OUTPUT-BUFFER

   retrieve the URL to PATH, with OUTPUT-BUFFER as the output
   buffer. The process object created is returned, or nil if a
   process could not be created.
  "
  (condition-case trapped-error
    (let
      ((default-directory (grail-garuntee-dir-path dir)))

      (start-process-shell-command "grail-hg" output-buffer
        "hg"
        "clone"
        (quote-string-for-shell url)
        (quote-string-for-shell module)))
    (error
      (progn
        (message "grail-hg-async failed %s" (format-signal-trap trapped-error))
        nil)) ))

(defun grail-hg-installer ( module url )
  (lexical-let
    ((module-arg  module)
     (url-arg     url))

    (grail-run-and-wait grail-hg-buffer
      (lambda ( run-buffer )
        (grail-hg-async url-arg grail-dist-hg module-arg run-buffer))) ))

(defun grail-hg-docs ( module url )
  (lexical-let
    ((module-arg  module)
     (url-arg     url))

    (grail-run-and-wait grail-hg-buffer
      (lambda ( run-buffer )
        (grail-hg-async url-arg grail-hg-docs module-arg run-buffer))) ))

;;
;; ELPA
;;

(defun grail-package-installer ( module )
  (package-install module))

;;----------------------------------------------------------------------
;; installer front ends
;;----------------------------------------------------------------------

;;
;; emacswiki
;;

;;
;; emacsmirror
;;

;;----------------------------------------------------------------------
;; grail-define-installer
;;----------------------------------------------------------------------

;; The arg helpers adapt the installer definition process to specific
;; installers.

(defun grail-target ( url-pair )
  (car url-pair))

(defun grail-url ( url-pair )
  (cdr url-pair))

(defun grail-make-pair ( target url )
  (cons target url))

;; From a uniform single URL argument parameter and the dynamic scoped
;; bindings of grail-define-installer they generate the installer
;; function calls with the parameters required by the installer
;; function signatures which vary based upon their specific needs.

(defun grail-file-args ( install-pair )
  (cons 'grail-file-url
    (if install-many
      ;; When there are multiple install pairs pass the package name
      ;; as a sub-directory to install the files in.

      ;; When there is a single install pair the target part needs to
      ;; have the .el extension added.
      `(,(grail-target install-pair) ,(grail-url install-pair) ,name)
      `(,(concat (grail-target install-pair) ".el") ,(grail-url install-pair)))))

(defun grail-tar-args ( install-pair )
  ;; When installing a local archive only the path and the compression
  ;; need be known, as the target directory and the like cannot be
  ;; ascertained without inspecting the archive.

  ;; for a remote archive pass the name, the url, and the
  ;; compression. The name is used for naming the download. This is
  ;; especially useful when the downloads are saved.
  (if (string-match "archived:\\(.*\\)" (grail-url install-pair))
    `(grail-tar-local-installer ,(concat grail-dist-archive (match-string 1 (grail-url install-pair))) ,compression)
    `(grail-tar-remote-installer ,(grail-target install-pair) ,(grail-url install-pair) ,compression)))

(defun grail-cvs-args ( install-pair )
  `(grail-cvs-installer ,(grail-target install-pair) ,(grail-url install-pair)))

(defun grail-git-args ( install-pair )
  `(grail-git-installer ,(grail-target install-pair) ,(grail-url install-pair)))

(defun grail-svn-args ( install-pair )
  `(grail-svn-installer ,(grail-target install-pair) ,(grail-url install-pair)))

(defun grail-bzr-args ( install-pair )
  `(grail-bzr-installer ,(grail-target install-pair) ,(grail-url install-pair)))

(defun grail-hg-args ( install-pair )
  `(grail-hg-installer ,(grail-target install-pair) ,(grail-url install-pair)))

(defun grail-decompose-installer-type ( type-spec )
  "grail-decompose-installer-type SPEC

   Spec is either a single value such as file|cvs, or a pair such
   as tar:bz2. When a pair is detected return it as a cons cell,
   or simply return the spec as given.
  "
  (let
    ((split-index (string-match ":" type-spec)))

    (if split-index
      (cons (substring type-spec 0 split-index) (substring type-spec (match-end 0)))
      type-spec)))

(defun grail-define-installer ( name type &rest url-list )
  "grail-define-installer NAME TYPE &rest URLS

   define a installer for a package NAME.

   The type of the installer indicates the format of the URL.

   TYPE is the format of the URL for handling things like
   compression,archives, and RCS systems.

   recognized TYPE's : file, tar:bz2, tar:gz, cvs svn git bzr pkg

   download a plain elisp file: (grail-define-installer \"bob\" \"file\" \"URL\")
   download an tar.bz2 archive: (grail-define-installer \"bob\" \"tar:bz2\" \"URL\")
   cvs checkout:              : (grail-define-installer \"bob\" \"cvs\" \"pserver\")
   git checkout:              : (grail-define-installer \"bob\" \"git\" \"url\")
   svn checkout:              : (grail-define-installer \"bob\" \"svn\" \"url\")
   bzr checkout:              : (grail-define-installer \"bob\" \"bzr\" \"url\")
   ELPA package:              : (grail-define-installer \"bob\" \"pkg\")
   hg   package:

   Most of the time a single URL suffices. Many packages are a
   single elisp file, or a single tarball.

   Other packages such as icicles are several elisp files, or
   possibly several archives.

   In this case a list of cons pairs can be given as the
   URL. When this happens NAME becomes a sub-directory they are
   installed to, and the files a list of (name . url) pairs.

   (grail-define-installer PACKAGE \"file\"
    '(\"foo.el\" . \"URL\")
    '(\"bar.el\" . \"URL\")

    this would install as:
    PACKAGE/foo.el
    PACKAGE/bar.el
  "
  (let
    ((install-many  (> (length url-list) 1))
     (install-type  (grail-decompose-installer-type type))
     (compression   nil))

    (when (consp install-type)
      (setq compression  (cdr install-type))
      (setq install-type (car install-type)))

    ;; do a bit more input checking than usual as the definitions are user inputs.

    (unless (and (stringp name) (> (length name) 0))
      (throw 'grail-trap
        '((format "installer expected package name string but got %s instead" (princ name)))))

    (if (string-equal "pkg" type)
      `(grail-package-installer ',(car url-list)) ;; a package system is the only form that won't have a url.
      (progn
        (unless url-list
          (throw 'grail-trap
            '((format "grail-define-installer: installer definition for %s must be given urls" name))))

        (let
          ((installer-calls
             (mapcar
               (lambda ( url )
                 (let
                   ;; simpler definitions don't require a target,url pair.
                   ;; make one up to prevent the test for and handling of
                   ;; this case from propagating down the fan-out from
                   ;; grail-define-installer.
                   ((install-pair (if (consp url) url (cons name url))))

                   (cond
                     ((string-equal "file"  install-type) (grail-file-args install-pair))
                     ((string-equal "cvs"   install-type) (grail-cvs-args  install-pair))
                     ((string-equal "git"   install-type) (grail-git-args  install-pair))
                     ((string-equal "svn"   install-type) (grail-svn-args  install-pair))
                     ((string-equal "bzr"   install-type) (grail-bzr-args  install-pair))
                     ((string-match "tar"   install-type) (grail-tar-args  install-pair))
                     ((string-match "hg"   install-type)  (grail-hg-args  install-pair))

                     (t (throw 'grail-trap
                          '((format "grail-define-installer: I don't have an installer for %s" install-type))))) ))
               url-list)))

          ;; if there are several call-outs sequence them with and so that
          ;; a failure terminates the install process. for a single
          ;; call-out extract the call from the map list and return it.
          (if install-many
            (cons 'and installer-calls)
            (car installer-calls)))) ) ))

(defun grail-load ( package installer )
  (grail-report-info "grail-load" "attempting to load the profile: " (pp-to-string package))

  (unless (symbolp package)
    (grail-signal-fail "grail-load" (format "package \"%s\" is not a symbol" (pp-to-string package))) )

  (grail-recover
    "grail-load"
    (format "attempting to load profile component %s" (symbol-name package))

    (grail-fail
      "grail-load retry"
      "attempting to recover from a failed profile load by installing"

      ;; try and force a unload ignoring errors hoping they are not blockers
      (grail-ignore
        "grail-load reset"
        "unload reset a failed profile"

        ;; force unload
        (unload-feature package t) )

      ;; try and install ignoring errors hoping they are not blockers
      (grail-ignore
        "grail-load install"
        "performing a install of a failed profile"

        ;; set default dir and archive strip options
        (grail-dist-default-to-packages)
        (grail-untar-strip-by-el)

        (grail-report-info "grail-load install" "running installer for" package)

        (cond
          ((functionp installer) (funcall installer))
          ((listp installer)     (eval installer))
          (t                     (grail-signal-fail "grail-load" "unknown installer type" package)) ) )

      (grail-report-info "grail-load install" "attempting reload for" package)

      (grail-update-load-path)
      (require package)

      (grail-report-info "grail-load" "profile loaded on install/retry" package)

      t)

    (require package)
    (grail-report-info "grail-load" "profile loaded on the first try" package) ))

(defun grail-fetch-docs ( top-level-dir installer strip-level )
  (let
    ((dir-path (concat grail-dist-docs top-level-dir)))

    (if (grail-dir-if-ok dir-path)
      dir-path
      (progn
        (grail-dist-default-to-docs)
        (grail-untar-strip-by-depth strip-level)

        (grail-run-installer installer)

        (if (dir-path-if-ok dir-path)
          dir-path
          nil))) ))

(provide 'grail-profile)
