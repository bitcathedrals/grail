;; -*- lexical-binding: t; no-byte-compile: t; -*-

(require 'thingatpt)
(require 'syntax-move)

(defvar dwim-tab-register-expand nil
  "dwim-tab function for expanding registers")

(defvar dwim-tab-global-context nil
  "A list of functions for contextualized-tab to try. These functions need to return t only
   if they are certain their dwim is the right dwim.")

(defvar-local dwim-tab-local-context nil
  "A function, or list of functions ")

(defvar-local dwim-tab-indent 'indent-according-to-mode)

(defun dwim-tab-set-register-expand ( expander )
  (setq dwim-tab-register-expand expander))

(defun dwim-tab/at-whitespace()
  "dwim-tab/at-whitespace

   return t if at whitespace, otherwise return nil
  "
  (let
    ((after (char-after)))

    (if (not after)
      t
      (if (equal nil (string-match "[\r\n\t\v\f ]" (char-to-string after)))
        nil
        t)) ))

(defun dwim-tab/in-word ()
  (interactive)

  (thing-at-point 'symbol t))

(defun dwim-tab/after-word ()
  (interactive)

  (and
    (thing-at-point 'symbol t)
    (dwim-tab/at-whitespace)) )

(defun dwim-tab/prefix ()
  (interactive)
  (save-excursion
    (let
      ((end (point)))

      (re-search-backward "^\\|\s+\\|[(\\[;\"]")

      (let*
        ((start (if (looking-at "\s+\\|[(\\[;\"]")
                  (+ (point) 1)
                  (point) ))
         (length (if (< start end)
                   (- end start)
                   0)))

        ;; (message "prefix is %d %d %s" start end (buffer-substring-no-properties start end))

        (if (> length 0)
          (list (buffer-substring-no-properties start end) start end length)
          nil) )) ))

(defun dwim-tab/stem (prefix)
  (if prefix
    (car prefix)
    nil))

(defun dwim-tab/start (prefix)
  (if prefix
    (nth 1 prefix)
    nil))

(defun dwim-tab/end (prefix)
  (if prefix
    (nth 2 prefix)
    nil))

(defun dwim-tab/length (prefix)
  (if prefix
    (nth 3 prefix)
    nil))

(defun dwim-tab-make-expander ( context expander )
  (cons context expander))

(defun dwim-tab-expanders-by-context ()
  (let
    ((all-expanders (append dwim-tab-local-context dwim-tab-global-context))
     (relevant-expanders nil) )

    (mapcar
      (lambda ( expander )
        (when (funcall (car expander))
          (setq relevant-expanders (cons (cdr expander) relevant-expanders))) )
      all-expanders)

    (if dwim-tab-register-expand
      (cons dwim-tab-register-expand relevant-expanders)
      relevant-expanders) ))

(defun dwim-tab-localize-context ( &rest locals )
  "dwim-tab-local-context context-list

   Add a context to the local Tab hook. If the context function detects
   that the point is within it's context, or turf then it should
   DTRT and return non-nil. The expansion function should expand
   at the point.
  "
  (apply 'add-to-list 'dwim-tab-local-context locals))

(defun dwim-tab-globalize-context ( &rest globals )
  "dwim-tab-globalize-context function

   Add a context to the global Tab hook. If the function detects that
   the point is within it's context, or turf then it should DTRT and
   return non-nil. The expander should expand at the point.
  "
  (apply 'add-to-list 'dwim-tab-global-context globals))

(defvar dwim-buffer-change-status nil)

(defun dwim-buffer-change-hook ( start end )
  (setq dwim-buffer-change-status t))

 (defun dwim-install-change-hook ()
   (setq dwim-buffer-change-status nil)
   (add-hook 'before-change-functions 'dwim-buffer-change-hook nil t))

 (defun dwim-remove-change-hook ()
   (remove-hook 'before-change-functions 'dwim-buffer-change-hook t))

(defun try-complete-dwim ()
  "try-complete-dwim COMPLETE

   Search through the COMPLETE function list for a expansion. This
   function catches 'terminate-complete.
  "
  (interactive)
  (let
    ((complete (dwim-tab-expanders-by-context))
     (point-before (point))
      (success nil))

    (when complete
      (dwim-install-change-hook)

      (setq dwim-buffer-change-status nil)

      (while complete
        (funcall (car complete))

        (setq complete
          (if dwim-buffer-change-status
            (progn
              (setq success t)
              nil)
            (cdr complete))) )

      (dwim-remove-change-hook))

      success))

(defun dwim-tab-hop ()
  (interactive)
  (let
    ((before (point)))

    (when (and
            (not (looking-back "^"))
            (thing-at-point 'word)
            (syntax-move/enabled))
      (syntax-move/forward)

      (if (not (equal before (point)))
        t
        nil) ) ))

(defun dwim-tab-do-magic ()
  "dwim-tab-do-magic FUNCTIONS

   1. try contextual (global,local) DTRT functions at the
      point stopping if a function succeeds.

   2.A when following non-whitespace try completion functions
     B otherwise indent according to the mode.

   The context functions are shared globally, while the
   completion functions are bound to the generated function.
  "
  (interactive)

  (unless (try-complete-dwim)
    (unless (dwim-tab-hop)
      (funcall dwim-tab-indent))) )


(defun turn-on-dwim-tab ( &optional indent-function )
  (interactive)

  (when indent-function
      (setq dwim-tab-indent indent-function))

  (keymap-local-set "<tab>" 'dwim-tab-do-magic))

(provide 'dwim-tab)
