;;;----------------------------------------------------------------------
;; grail-profile.el
;;----------------------------------------------------------------------
(require 'cl)
(require 'async-exec)
(require 'sync-exec)

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
    (mapc
     (lambda ( str )
       (insert (pp-to-string str))
       (end-of-line))
     info)

    (end-of-line)
    (newline) ))

(defun grail-load-profile ( profile )
  (grail-info "grail: loading [profile] ->  " profile)

  (let
    ((path (concat grail-local-profiles "/"  profile ".el") ))

    (grail-info "grail: loading [path] -> " path)

    (condition-case trap
      (progn
        (load (expand-file-name path))
        (grail-info "grail: [OK] " profile)
        (push profile grail-ok))
      (error
       (progn
         (grail-info "grail: [FAIL!] " profile " [error]: " (car trap) " [details]: " (cdr trap))
         (push profile grail-failed))) ) ))


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
          (grail-info "grail-load-all-profiles: [" (car level) "] [profiles]: " (pp-to-string (cdr level)))

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
  (grail-info "grail: adding [" (pp-to-string order) "]: " (pp-to-string requested)) 

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
