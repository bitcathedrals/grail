;;----------------------------------------------------------------------
;; dwim-complete.el - advanced global completion system
;;
;; description:
;;
;; a completion system designed to be used as a engine for multiple
;; modules and as a interface that is generally usable throughout Emacs.
;; currently helm is outstanding for this purpose.
;;----------------------------------------------------------------------
(require 'thingatpt)
(require 'subr-x)
(require 'helm)

;;
;; options
;;

(setq helm-execute-action-at-once-if-one t)

(defvar dwim-complete-fuzzy-match t)

;;
;; interface to helm
;;

(defun dwim-complete/helm ( prompt input helm-buffer helm-source )
  (helm
    :sources helm-source
    :input input
    :prompt prompt
    :buffer helm-buffer))


(defvar dwim-complete-stem-start nil)
(defvar dwim-complete-stem-stop nil)

(defun dwim-complete-set-stem ( start end )
  (setq dwim-complete-stem-start start)
  (setq dwim-complete-stem-stop end))

(defun dwim-complete-replace-stem ( completion )
  (when (and
          (and dwim-complete-stem-start dwim-complete-stem-stop)
          (> (- dwim-complete-stem-stop dwim-complete-stem-start) 0))

    (delete-region dwim-complete-stem-start dwim-complete-stem-stop)
    (goto-char dwim-complete-stem-start) )

  (insert (format "%s" completion)))

(defun dwim-complete-delete-stem ()
  (when (and
          (and dwim-complete-stem-start dwim-complete-stem-stop)
          (> (- dwim-complete-stem-stop dwim-complete-stem-start) 0))

    (delete-region dwim-complete-stem-start dwim-complete-stem-stop)
    (goto-char dwim-complete-stem-start)) )

(defun dwim-complete-behind-point ()
  (interactive)

  (let
    ((stem (thing-at-point 'symbol) )
     (start nil )
     (end (point)))

    (if stem
      (progn
        (dwim-complete-set-stem (- (point) (length stem)) end)
        stem)
      "") ))

;;
;; dwim-complete source data type
;;

(defun dwim-complete/make-name (name)
  `("name" . ,name))

(defun dwim-complete/make-completions ( completions-fn )
  `("candidates" . completions-fn))

(defun dwim-complete/make-action ( &optional fn )
  `("action" . ,(if fn fn (lambda (selection) selection))))

(defun dwim-complete/make-source ( name completions-fn action )
  (list
    (dwim-complete/make-name name)
    (dwim-complete/make-completions completions-fn)
    (dwim-complete/make-action action)))

;;
;; accessor functions
;;

(defun dwim-complete-get-name (source)
  (assoc "name" source))

(defun dwim-complete-get-completions (source)
  (assoc "candidates" source))

(defun dwim-complete-get-action (source)
  (assoc "action" source))

;;
;; dwim-complete sources data table
;;

(defvar dwim-complete-mode-sources nil "Global dwim-complete sources")
(defvar-local dwim-complete-local-sources nil "local dwim-complete sources")

(defun dwim-complete-mode-add-source ( mode-name source &optional local )
  (if local
    (setq dwim-complete-local-sources (cons source dwim-complete-local-sources))
    (let
      ((global (assoc mode-name dwim-complete-mode-sources)))

      (if global
        (setcdr global (cons source (cdr global)))
        (setq dwim-complete-mode-sources
          (cons `(,mode-name . ,(list source)) dwim-complete-mode-sources)) )) ))

(defun dwim-complete-get-sources (mode-name select-field)
  (let
    ((merged-table (assoc mode-name dwim-complete-mode-sources))
     (completers nil))

    (when dwim-complete-local-sources
      (setq merged-table (append dwim-complete-local-sources merged-table)))

    (when merged-table
      (mapcar
        (lambda ( source )
          (setq completers (cons (select-field source) completers)))
        merged-table))

    completers))

;;
;; build helm source from sources table
;;

(defun dwim-complete/buffer ()
  (get-buffer-create "*complete*"))

(defun dwim-complete-build-candidates ()
  (let
    ((completers (dwim-complete-get-sources major-mode 'dwim-complete-get-completions)))
    ((completions nil))

    (when completers
      (mapcar
        (lambda ( complete )
          (setq completions (append (complete) completions)))
        completers)

      (sort completions 'string-lessp)) ))

(defun dwim-build-helm-source ()
  (let
    ((candidates (dwim-complete-build-candidates)))

    (when candidates
      (helm-build-sync-source major-mode
        :candidates (dwim-complete-build-candidates)
        :fuzzy-match dwim-complete-fuzzy-match
        :buffer (dwim-compete/buffer))) ))


(defun dwim-complete/complete ()
  "Attempt to perform a completion of what is behind the cursor."
  (interactive)
  (let
    ((helm-source (dwim-build-helm-source)))

    (if helm-source
      (dwim-complete/helm
        "complete: "                  ;; prompt
        (dwim-complete-behind-point)  ;; input
        (dwim-complete/buffer)        ;; buffer
        helm-source                   ;; sources
        )
      (message "no completions available for mode: %s" (major-mode)) )))

(defun dwim-complete/for-buffer ()
  (make-local-variable 'dwim-complete-stem-start)
  (make-local-variable 'dwim-complete-stem-stop)

  (local-set-key (kbd "<M-tab>") 'dwim-complete/complete) )

;;----------------------------------------------------------------------
;; keybindings and interfaces.
;;----------------------------------------------------------------------

(defun dwim-complete-vcs-or-file ()
  "dwim-complete vcs for file completion: use <spc> for contents search."
  (interactive)
  (helm-browse-project))

(provide 'profile/dwim-complete)
