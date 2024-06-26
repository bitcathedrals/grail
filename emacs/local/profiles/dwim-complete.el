;; -*-no-byte-compile: t; -*-

(require 'thingatpt)
(require 'subr-x)
(require 'lex-cache)
(require 'dwim-tab)

(require 'helm)

;;
;; options
;;

(make-variable-buffer-local 'dwim-complete-buffer-mode)

(setq-default helm-execute-action-at-once-if-one t)

(defvar dwim-complete-fuzzy-match t)

;;
;; interface to helm
;;

(defun dwim-complete-buffer ()
  (get-buffer-create "*complete*"))

(defun dwim-complete/helm ( prompt input source-list )
  (let
    ((complete (helm
                 :sources source-list
                 :input input
                 :prompt prompt
                 :buffer (dwim-complete-buffer))))

    (if (stringp complete)
      (dwim-complete-replace-stem complete)
      (message "no completion.")) ))

;;
;; dwim input construction
;;

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
    (goto-char dwim-complete-stem-start))

  (message (concat "completion is: " completion))

  (insert completion))

(defun dwim-complete-behind-point ()
  (interactive)
  (let
    ((prefix (dwim-tab/prefix)))

    (if prefix
      (progn
        (dwim-complete-set-stem (dwim-tab/start prefix) (dwim-tab/end prefix))
        (dwim-tab/stem prefix))
      nil)) )

;;
;; dwim-complete source data type
;;

(defun dwim-complete/make-name (name)
  `("name" . ,name))

(defun dwim-complete/make-completions ( completions-fn )
  `("candidates" . (sort ,completions-fn 'string-lessp)) )

(defun dwim-complete/make-source (name completions-fn)
  (list
    (dwim-complete/make-name name)
    (dwim-complete/make-completions completions-fn) ))

;;
;; accessor functions
;;

(defun dwim-complete-get-name (source)
  (cdr (assoc "name" source)))

(defun dwim-complete-get-completions (source)
  (cdr (assoc "candidates" source)))

;;
;; dwim-complete sources data table
;;

(defvar dwim-complete-mode-sources nil "Global dwim-complete sources")
(defvar-local dwim-complete-local-sources nil "local dwim-complete sources")

(defun dwim-complete-get-sources (mode)
  (let
    ((merged-table (assoc mode dwim-complete-mode-sources)))

    (when merged-table
      (setq merged-table (cdr merged-table)))

    (when dwim-complete-local-sources
      (setq merged-table (append dwim-complete-local-sources merged-table)))

    merged-table))

;;
;; helm source cache
;;

(defconst dwim-complete-refresh-interval 5 "how many seconds between refreshes of dwim-complete data")

(defvar-local dwim-complete-build-generator
  (lambda ()
    nil)
  "dwim-complete-build-generator is a function that generates a list of helm source objects")

(defvar-local dwim-complete-local-fetch
  (lex-cache-lambda
    dwim-complete-refresh-interval
    (lambda ()
      (append
        (dwim-complete-build-helm-from-mode dwim-complete-buffer-mode)
        (funcall dwim-complete-build-generator)) )) )

;;
;; helm builders
;;

(defun dwim-complete-build-helm-from-source ( source )
  (helm-build-sync-source (dwim-complete-get-name source)
    :candidates (funcall (dwim-complete-get-completions source))
    :fuzzy-match dwim-complete-fuzzy-match))


(defun dwim-complete-build-helm-from-generator ( name completions )
  (helm-build-sync-source name
    :candidates completions
    :fuzzy-match dwim-complete-fuzzy-match))

(defun dwim-complete-build-helm-from-mode ( mode )
  (mapcar
    (lambda (table-entry)
      (dwim-complete-build-helm-from-source table-entry))
      (dwim-complete-get-sources mode)) )

(defun dwim-complete/mode-add ( mode source &optional local )
  (if local
    (setq dwim-complete-local-sources (cons source dwim-complete-local-sources))
    (let
      ((global (assoc mode dwim-complete-mode-sources)))

      (if global
        (setcdr global (cons source (cdr global)))
        (setq dwim-complete-mode-sources (list `(,mode . ,(list source)))) ) )) )

;;
;; completion command
;;

(defun dwim-complete/complete ()
  "Attempt to perform a completion of what is behind the cursor."
  (interactive)
  (dwim-complete/helm
    "complete: "                            ;; prompt
    (dwim-complete-behind-point)            ;; input
    (funcall dwim-complete-local-fetch)) )  ;; sources

;;
;; dwim-tab integration
;;

(defun dwim-complete-make-context ()
  (cons
   'dwim-tab/after-word
   'dwim-complete/complete))

(defun dwim-complete/setup-for-buffer ( mode &optional generator )
  (setq dwim-complete-buffer-mode mode)

  (when generator
    (setq dwim-complete-build-generator generator))

  (make-local-variable 'dwim-complete-stem-start)
  (make-local-variable 'dwim-complete-stem-stop)

  (dwim-tab-localize-context (dwim-complete-make-context)) )

(provide 'profile/dwim-complete)
