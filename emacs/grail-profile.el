;; -*-no-byte-compile: t; -*-

(require 'subr-x)

(defvar grail-masked
  nil
  "List of grail profiles masked by the user.")

(defvar grail-loading
  nil
  "List of grail profiles to load.")

(defvar grail-failed
  nil
  "List of grail profiles that FAILED to load.")

(defvar grail-ok
  nil
  "List of grail profiles that have loaded OK.")

;;
;; grail-installer tools
;;

(defconst grail-profile-buffer-name "*grail-loader*")

(defun grail-profile-buffer ()
  (get-buffer-create grail-profile-buffer-name))

(defun grail-show-buffer ()
  (interactive)
  (pop-to-buffer (grail-profile-buffer)))

(defun grail-info ( &rest info )
  (with-current-buffer (grail-profile-buffer)
    (insert (string-join (mapcar
                          (lambda (elt)
                            (if (stringp elt)
                                elt
                              (pp-to-string elt)))
                          info) ","))
    (end-of-line)
    (newline)))

(defvar grail-profile/loaded (make-hash-table :test 'equal))

(defun grail-load-profile ( profile )
  (let
    ((path (expand-file-name (concat grail-local-profiles "/"  profile ".el")) )
     (info nil))

    (if (gethash path grail-profile/loaded)
      (push (format "grail: skipping already loaded profile [%s]" profile) info)
      (condition-case trap
        (progn
          (push (format "grail: loading [%s] -> %s" profile path) info)

          (load path)

          (push "grail: load [OK]" info)
          (push profile grail-ok)

          (puthash path t grail-profile/loaded))
        (error
          (progn
            (push (format "grail: [FAIL!] %s [details] %s"
                    (pp-to-string (car trap))
                    (pp-to-string (cdr trap)))
              info)
            (push profile grail-failed))) ) )

    (grail-info (reverse info)) ))

(defun grail-load-all-profiles ()
  "grail-load-requested-profiles

   Load the profiles in the request list grail-requested-profiles.
  "
  (interactive)
  (when grail-loading
    (let
      ((sorted (sort grail-loading (lambda ( a b )
                                     (if (< (car a) (car b)) t) )) ))
      (mapc
        (lambda ( level )
          (grail-info (format "grail-load-all-profiles: [%s] -> %s"
                        (car level)
                        (string-join (cdr level) ",")))

          (mapc
            (lambda ( profile )
              (let
                ((filtered '()))

                (mapc
                  (lambda ( profile )
                    (when (not (member profile grail-masked))
                      (setq filtered (cons profile filtered)) ))
                  (cdr level))

                (mapc 'grail-load-profile filtered) ))
            (cdr level)) )
        sorted) )))

(defun use-grail-profiles ( order &rest requested )
  "use-grail-groups: ORDER LIST

   request a list of string quoted groups to be loaded after the configuration
   files have been loaded.
  "
  (grail-info (format "grail: adding [%d]: %s " order (string-join requested ",") ) )

  (let
    ((combined '()))

    (mapc
     (lambda ( level )
       (when (equal (car level) order)
         (setq combined (append requested (cdr level))) ))
         grail-loading)

    (if combined
      (setq grail-loading (mapcar
                            (lambda ( level )
                              (if (equal (car level) order)
                                (cons order combined)
                                level))
                            grail-loading))
      (setq grail-loading (cons (cons order requested) grail-loading)) ) ))

(defun mask-grail-profiles ( &rest mask )
  "mask-grail-profiles: LIST

   mask profiles to not be loaded.
  "
  (setq grail-masked (append mask grail-masked)))

(provide 'grail-profile)
