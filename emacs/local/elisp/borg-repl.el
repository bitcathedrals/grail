;; -*-no-byte-compile: t; -*-

(require 'buffer-ring)

(make-variable-buffer-local 'borg-repl/repl-name)
(make-variable-buffer-local 'borg-repl/create-repl)
(make-variable-buffer-local 'borg-repl/eval-line)
(make-variable-buffer-local 'borg-repl/eval-region)
(make-variable-buffer-local 'borg-repl/eval-buffer)
(make-variable-buffer-local 'borg-repl/eval-defun)
(make-variable-buffer-local 'borg-repl/get-buffer)

(defun borg-repl/repl-name ( mode-name )
  (concat mode-name "/repl") )

(defun borg-repl/error-msg ( msg &rest info )
  (message "borg-repl Error: %s %s" msg
    (if info
      (concat "[ " (string-join (mapcar
                                 (lambda (x)
                                   (if (stringp x)
                                     x
                                     (pp-to-string x)))
                                 info) ", ") " ]")
      "")))

(defun borg-repl/invoke-binding ( binding )
  (if (symbol-function binding)
    (if (commandp binding)
      (let
        ((current-prefix-arg nil))
        (call-interactively binding))
      (funcall binding) )
    (borg-repl/error-msg  "borg-repl: no binding for - " (symbol-name binding)) ))

(defun borg-repl/start ()
  "borg-repl/start

   start the REPL this buffer is configured for.
  "
  (interactive)
  (borg-repl/invoke-binding 'borg-repl/create-repl))

(defun borg-repl/connect ()
  "borg-repl/connect

   connect to a running repl.
  "
  (interactive)
  (borg-repl/invoke-binding 'borg-repl/connect-to))

(defun borg-repl/statement ()
  "borg-repl/statement

   eval the last statement.
  "
  (interactive)
  (borg-repl/invoke-binding 'borg-repl/eval-line))

(defun borg-repl/region ()
  "borg-repl/region

   eval the region.
  "
  (interactive)
  (borg-repl/invoke-binding 'borg-repl/eval-region))

(defun borg-repl/macro-expand ()
  "borg-repl/macro-expand

   perform a macro expand at the point
  "
  (interactive)
  (borg-repl/invoke-binding 'borg-repl/macro-expand))

(defun borg-repl/buffer ()
  "borg-repl/buffer

   eval the buffer.
  "
  (interactive)
  (borg-repl/invoke-binding 'borg-repl/eval-buffer))

(defun borg-repl/definition ()
  "borg-repl/definition

   eval the definition.
  "
  (interactive)
  (borg-repl/invoke-binding 'borg-repl/eval-defun))

(defun borg-repl/get-buffer ()
  "borg-repl/get-buffer

   get the current repl buffer for this buffer performing all the null
   checks along the way.
  "
  (let
    ((repl-buffer (local-or-nil 'borg-repl/get-repl-buffer (current-buffer))))

    (if repl-buffer
      (funcall repl-buffer)
      (if (local-or-nil 'borg-repl/repl-name (current-buffer))
        (let
          ((repl-buffer (buffer-torus/get-ring-buffer borg-repl/repl-name) ))

          (if repl-buffer
            (progn
              (setq repl-buffer (get-buffer repl-buffer))

              (if repl-buffer
                repl-buffer
                (progn
                  (borg-repl/error-msg "ghost REPL buffer does not exist anymore. will clean REPL ring.")
                  (buffer-ring/fix (buffer-torus/search-rings borg-repl/repl-name))
                  nil)) )
            (progn
              (borg-repl/error-msg "no REPL in this ring. try creating one.")
              nil)) )
        (progn
          (borg-repl/error-msg "no REPL for this buffer")
          nil) )) ))

(defun borg-repl/view ()
  "borg-repl/view

   view the repl without switching to the repl.
  "
  (interactive)

  (let
    ((repl-buffer (borg-repl/get-buffer) ))

    (when repl-buffer
      (pop-to-buffer repl-buffer) ) ))

(defun borg-repl/switch ()
  "borg-repl/switch

   switch to the repl
  "
  (interactive)

  (let
    ((repl-buffer (borg-repl/get-buffer) ))

    (when repl-buffer
      (pop-to-buffer repl-buffer)
      (switch-to-buffer repl-buffer) ) ))

(defun borg-repl/next ()
  "borg-repl/next

   rotate the REPL ring right.
  "
  (interactive)

  (if (local-or-nil 'borg-repl/repl-name (current-buffer))
    (if repl-ring
      (buffer-ring/next repl-ring)
      (progn
        (borg-repl/error-msg "no REPL in this ring. try creating one.")
        nil))
    (progn
      (borg-repl/error-msg "no REPL for this buffer")
      nil)) )

(defun borg-repl/prev ()
  "borg-repl/prev

   rotate the REPL ring left.
  "
  (interactive)

  (if (local-or-nil 'borg-repl/repl-name (current-buffer))
    (funcall borg-repl/repl-name)
    (progn
      (borg-repl/error-msg "no REPL for this buffer")
      nil) ) )

(defun borg-repl/no-defun ()
  (interactive)
  (message "no defun evaluator defined for mode [%s]" borg-repl/repl-name))

(defun borg-repl/no-buffer-get ()
  (interactive)
  (message "no buffer-get defined for mode [%s]" borg-repl/repl-name))

(defun borg-repl/bind-repl (repl-name
                            create-fn
                            line-fn
                            region-fn
                            buffer-fn
                            defun-fn
                            get-buffer)

  (set  'borg-repl/repl-name    repl-name)
  (fset 'borg-repl/create-repl  (symbol-function create-fn))
  (fset 'borg-repl/eval-line    (symbol-function line-fn))
  (fset 'borg-repl/eval-region  (symbol-function region-fn))
  (fset 'borg-repl/eval-buffer  (symbol-function buffer-fn))

  (fset 'borg-repl/eval-defun (or (symbol-function defun-fn) #'borg-repl/no-defun))
  (fset 'borg-repl/get-buffer (or (symbol-function get-buffer) #'borg-repl/no-buffer-get))

  (custom-key-group "borg repl" "e" nil
    ("x" . borg-repl/start)
    ("c" . borg-repl/connect)
    ("e" . borg-repl/statement)
    ("r" . borg-repl/region)
    ("b" . borg-repl/buffer)
    ("d" . borg-repl/definition)
    ("v" . borg-repl/view)
    ("s" . borg-repl/switch)
    ("<left>" . borg-repl/next)
    ("<right>" . borg-repl/prev)
    ("m" . borg-repl/macro-expand)) )

(defun borg-repl/bind-macro-expand ( expand-fn )
  (set (make-local-variable 'borg-repl/macro-expand) expand-fn))

(defun borg-repl/bind-connect ( connect-fn )
  (set (make-local-variable 'borg-repl/connect-to) connect-fn) )

(provide 'borg-repl)
