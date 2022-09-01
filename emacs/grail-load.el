;;;----------------------------------------------------------------------
;; grail-load.el
;;----------------------------------------------------------------------

;;
;; grail-match - return a list of paths that match a given criteria
;;

(defun grail-elisp-files-only ( path-arg )
  (directory-files path-arg t ".*\.elc?$"))

(defun grail-subdirs-only ( path-arg &optional test )
  (let
    ((dirs '()))

    (mapc
      (lambda ( path )
        (let
          ((name (file-name-base path)))

          (when (and (not (string-equal "." name))
                     (not (string-equal ".." name))
                     (file-directory-p path)
                  (if test (funcall test path) t))
            (setq dirs (cons path dirs))) ))

      (directory-files path-arg t))

    dirs))

(defun grail-dirs-recurse ( dir &optional test )
  (if (and dir (file-directory-p dir))
      (apply 'append (list dir) (mapcar 'grail-dirs-recurse (grail-subdirs-only dir test)))
    '() ))

(defun grail-new-load-path ( &rest libraries )
  (let
      ((elisp-dirs '())
       (lib-list  nil)
       (verified '()))

    (mapc
       (lambda ( lib-arg )
         (if (listp lib-arg)
           (setq lib-list (append lib-arg lib-list))
           (setq lib-list (cons lib-arg lib-list)) ))
      libraries)

    (mapc
      (lambda ( lib-arg )
       (when (file-directory-p lib-arg)
         (setq verified (cons lib-arg verified))) )
      lib-list)

    (mapc
     (lambda ( lib-dir )
       (progn
         (when (grail-elisp-files-only lib-dir)
           (setq elisp-dirs (cons lib-dir elisp-dirs)))

         (let
           ((sub-dirs (apply 'append
                        (mapcar
                          (lambda ( nested-dir )
                            (grail-dirs-recurse nested-dir 'grail-elisp-files-only))
                          (grail-subdirs-only lib-dir)) )))
           (setq elisp-dirs (append sub-dirs elisp-dirs)) )))
      verified)

    (let
      ((final-path-list elisp-dirs))

      (when grail-platform-load-path
        (setq final-path-list (append grail-platform-load-path elisp-dirs)))

      (setq load-path final-path-list))

    verified))

;;
;; keep a table of all the dirs where we install so we can later
;; lookup by package a path to get misc data files out and see
;; if we have installed a package vs. it being built-in to emacs.
;;

(defvar grail-library-table (make-hash-table :test 'equal))

(defun grail-update-library-dirs ( library-list )
  (mapc
    (lambda ( lib-dir )
      (unless (gethash (file-name-base lib-dir) grail-library-table)
        (puthash (file-name-base lib-dir) lib-dir grail-library-table)) )
    library-list))

(defun grail-update-library-files ( library-dir )
  (when (file-directory-p library-dir)
    (mapc
     (lambda ( elisp-file )
       (unless (gethash (file-name-sans-extension elisp-file) grail-library-table)
         (puthash (file-name-sans-extension elisp-file) elisp-file grail-library-table)))
     (grail-elisp-files-only library-dir)) ))

(defun grail-update-load-path ()
  (interactive)
  "grail-update-load-path

   update the load path. all of the paths listed are scanned for sub-dirs and
   added. everything is pre-pended to grail-platform-load-path.
  "
  (let
    ((libraries
      (grail-new-load-path
        ;;
        ;; my code smells like roses so load first.
        ;;
        grail-local-elisp

        ;;
        ;; fresh elisp
        ;;
        grail-dist-elisp


        ;;
        ;; fresh version control
        ;;
        grail-dist-git

        ;;
        ;; ELPA kinda stale
        ;;
        grail-elpa-load-path)))

    (grail-update-library-dirs libraries)

    (grail-update-library-files grail-local-elisp)
    (grail-update-library-files grail-dist-elisp) ))

(defun grail-install-sentinel ( package )
  (gethash package grail-library-table))

;;
;; user interface loading
;;

(defun is-frame-gui ( &optional this-frame )
  "is-current-frame-x FRAME

   Return t if FRAME or (selected-frame) is a GUI frame, nil
   otherwise.
  "
  (let
    ((frame-type (framep this-frame)))

    (when (and frame-type
            (or
              (equal 'x frame-type)
              (equal 'w32 frame-type)
              (equal 'ns frame-type)))
      t) ))

;;
;; frame configuration before first frame
;;

(defvar grail-display-configured nil
  "Boolean for if grail has configured the display.")

(defun grail-configure-display ()
  "grail-configure-display

   Load the display configuration before the initial frame.
   This is done with a sentry so it only runs once.
  "
  (unless grail-display-configured
    (grail-try-user-elisp "configure-display")
    (setq grail-display-configured t) ))

(defun grail-merge-frame-parameters ( emacs-alist-sym &rest pairs )
  (let
    (( emacs-alist (eval emacs-alist-sym) ))

    (mapc
      (lambda ( x )
        (setq emacs-alist (assq-delete-all (car x) emacs-alist)) )
      pairs)

    (set emacs-alist-sym (append pairs emacs-alist)) ))

;;
;; frame loading after first frame.
;;

(defvar grail-display-loaded nil
  "Boolean for if grail has configured the frame.")

(defun grail-load-display ( new-frame )
  "grail-load-display

   load the display after the first frame is created and
   the graphical related symbols have been defined.
  "
  (when (is-frame-gui new-frame)
    (when (not grail-display-loaded)
      ;; set best font
      (setq grail-font-best (grail-format-font (grail-select-best-font-family new-frame) grail-font-size new-frame))

      (when grail-font-best
        ;; set for all new frames
        (grail-merge-frame-parameters 'default-frame-alist `(font . ,grail-font-best))

        ;; set the best font for current frame
        (grail-set-best-font new-frame) )

      (eval-after-load 'cperl-mode '(display-faces-for-cperl))
      (eval-after-load 'ediff-mode '(display-faces-for-ediff))
      (eval-after-load 'whitespace-mode '(display-faces-for-whitespace-mode))
      (eval-after-load 'web-mode '(display-faces-for-web-mode))
      (eval-after-load 'helm '(display-faces-for-helm))
      (eval-after-load 'mic-paren '(display-faces-for-paren))
      (eval-after-load 'flyspell '(display-faces-for-flyspell))
      (eval-after-load 'term '(display-faces-for-term)) )

      (setq grail-display-loaded t))

    (let
      ((grail-frame new-frame))

      (grail-try-user-elisp "configure-frame")) )

;;
;; font stuff
;;

(defvar grail-font-best nil)

(defun grail-select-best-font-family ( frame )
  (catch 'best-font
    (dolist (canidate grail-font-family)
      (when (member canidate (font-family-list frame))
        (throw 'best-font canidate)) )

    (car (font-family-list frame)) ))

(defun grail-format-font ( family size frame )
  (let
    (( font-spec (aref (font-info family frame) 1) ))
    (concat (replace-regexp-in-string
              "pixelsize=[[:digit:]]+"
              (format "pixelsize=%s" size) font-spec)
      ":spacing=m") ))

(defun grail-set-best-font ( frame )
  (set-frame-parameter frame 'font grail-font-best)

  (set-face-attribute 'default frame
    :font grail-font-best) )

;;
;; ELPA
;;

(defun load-elpa-when-installed ()
  "load-elpa-when-installed

   If the ELPA package management system http://tromey.com/elpa/ is installed,
   configure it for use, assuming a proper install by grail-install-elpa.

   t is returned if succesful, otherwise nil is returned.
  "
  (interactive)
  (if (< emacs-major-version 24)
    (grail-report-info "grail-load" "cannot load ELPA min Emacs ver. is 24" emacs-major-version)
    (progn
      (setq-default package-user-dir (grail-dir-always grail-dist-elpa))

      ;; ELPA is loaded so do the ugly parts and hook into package.el's guts
      ;; to pick up it's modifications to load-path

      (defadvice package-activate-1 (after grail-snoop/do-activate)
        (let
          ((snooped (car load-path))) ;; elpa always cons's the new path on the front.

          (when snooped
            (message "grail: snooped load-path update %s from package.el" snooped)
            (setq grail-elpa-load-path (cons snooped grail-elpa-load-path))
            (grail-update-load-path)) ))

      (ad-activate 'package-activate-1)

      (grail-fail
        "grail-load ELPA"
        "running package initialize to trigger a ELPA package load"
        (package-initialize)) ) ))

(provide 'grail-load)
