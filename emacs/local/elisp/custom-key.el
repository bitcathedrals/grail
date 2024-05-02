;; -*- lexical-binding: t; no-byte-compile: t; -*-

(require 'subr-x)

(defvar custom-key-prefix "C-c"
  "default key chord for custom keys")

(defun keybindings-help-first-line ( fn )
  (if (functionp fn)
    (let
      ((docs (documentation fn)))

      (if docs
        (car
          (split-string docs "
"))
        "?"))
    "?"))

(defun keybindings-local-display ( group-name table )
  (format "key set: %s
%s"
    group-name
    (string-join
      (mapcar
        (lambda ( table-pair )
          (format "(%s) %s"
            (car table-pair)
            (keybindings-help-first-line (cdr table-pair))) )
        (cdr table))
      "
") ))

(defun keybindings-global-display ( table )
  (string-join
    (mapcar
      (lambda ( table-pair )
        (format "(%s) %s"
          (car table-pair)
          (keybindings-help-first-line (cdr table-pair))) )
      (cdr table))
    "
"))

(defconst keybindings-help-buffer-name "*keybindings help*")

(defvar custom-keys-table (make-hash-table :test 'equal)
  "descriptions of custom key groups")


(defun keybindings-help-quit ()
  (interactive)

  (other-window 1)
  (delete-other-windows)

  (kill-buffer (get-buffer keybindings-help-buffer-name)) )

(defun keybindings-help-local ( group-name table )
  (let
    ((doc-string (keybindings-local-display group-name table)))

    (lambda ()
      "help"
      (interactive)

      (switch-to-buffer
        (with-current-buffer
          (pop-to-buffer
            (get-buffer-create keybindings-help-buffer-name))

          (insert doc-string)

          (keymap-local-set "q" 'keybindings-help-quit)
          (message "press \"q\" to quit help.")

          (current-buffer))) )) )

(defun keybindings-help-global ()
  "display keybinding help"
  (interactive)

  (switch-to-buffer
    (with-current-buffer
      (pop-to-buffer
        (get-buffer-create keybindings-help-buffer-name))

      (erase-buffer)
      (insert "Custom Key Groups\n")

      (maphash
        (lambda ( key x )

          (insert (format (concat "keys: " custom-key-prefix " <%s> %s\n%s\n\n")
                    (elt x 0)
                    (elt x 1)
                    (keybindings-global-display (elt x 2)) )) )
        custom-keys-table)

      (keymap-local-set "q" 'keybindings-help-quit)
      (message "press \"q\" to quit help.")

      (current-buffer) )) )

(defun custom-key-group-new (chord description table)
  (vector chord description table))

(defun custom-key-group-register (chord description table)
  (unless (gethash chord custom-keys-table)
    (puthash chord (custom-key-group-new chord description table) custom-keys-table)) )

(defmacro custom-key-group ( description chord global &rest body )
  `(progn
     ;;
     ;; unbind everything
     ;;

     ,@(mapcar
         (lambda ( key-fn-pair )
           `(keymap-local-unset ,(concat custom-key-prefix " " chord " " (car key-fn-pair)) ))
         body)

     ;; bind the keys

     ,@(mapcar
         (lambda ( key-fn-pair )
           `(,(if global
                 'keymap-global-set
                 'keymap-local-set)

              ,(concat custom-key-prefix " " chord " " (car key-fn-pair))

              ,(if (symbol-function (cdr key-fn-pair))
                 `',(cdr key-fn-pair)
                 (cdr key-fn-pair)) ))
         body)

     ;; register for global help
     (custom-key-group-register ,chord ,description ',body)

     ;; set a help key for this group
     (keymap-local-set (concat custom-key-prefix " " ,chord " h") (keybindings-help-local ,description ',body))
     ))

(custom-key-group "help" "h" t
  ("a" . apropos)
  ("v" . describe-variable)
  ("f" . describe-function)
  ("k" . describe-key)
  ("g" . keybindings-help-global))

(provide 'custom-key)
