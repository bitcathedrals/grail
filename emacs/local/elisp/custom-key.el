;; -*- lexical-binding: t; no-byte-compile: t; -*-

(require 'subr-x)

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

(defun keybindings-local-display ( group-name keymap )
  (format "key set: %s
%s"
    group-name
    (string-join
      (mapcar
        (lambda ( keymap-pair )
          (format "(%s) %s"
            (char-to-string (car keymap-pair))
            (keybindings-help-first-line (cdr keymap-pair))) )
        (cdr keymap))
      "
") ))

(defun keybindings-global-display ( keymap )
  (string-join
    (mapcar
      (lambda ( keymap-pair )
        (format "(%s) %s"
          (char-to-string (car keymap-pair))
          (keybindings-help-first-line (cdr keymap-pair))) )
      (cdr keymap))
    "
") )

(defconst keybindings-help-buffer-name "*keybindings help*")

(defun keybindings-help-quit ()
  (interactive)

  (other-window 1)
  (delete-other-windows)

  (kill-buffer (get-buffer keybindings-help-buffer-name)) )

(defun keybindings-help-local ( group-name keymap )
  (let
    ((doc-string (keybindings-local-display group-name keymap)))

    (lambda ()
      "help"
      (interactive)

      (switch-to-buffer
        (with-current-buffer
          (pop-to-buffer
            (get-buffer-create keybindings-help-buffer-name))

          (insert doc-string)

          (local-set-key (kbd "q") 'keybindings-help-quit)
          (message "press \"q\" to quit help.")

          (current-buffer))) )) )

(defvar custom-keys-descriptions (make-hash-table :test 'equal)
  "descriptions of custom key groups")

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

          (insert (format "keys: C-c <%s> %s\n%s\n\n"
                    (elt x 0)
                    (elt x 1)
                    (keybindings-global-display (elt x 2)) )) )
        custom-keys-descriptions )

      (local-set-key (kbd "q") 'keybindings-help-quit)
      (message "press \"q\" to quit help.")

      (current-buffer) )) )

(defun custom-key-group-new (chord description keymap)
  (vector chord description keymap) )

(defun custom-key-group-register ( chord description key-map)
  (unless (gethash chord custom-keys-descriptions)
    (puthash chord (custom-key-group-new chord description key-map) custom-keys-descriptions)) )

(defvar custom-key-group-prefix "C-c"
  "default key chord for custom keys")

(defmacro custom-key-group ( description chord global &rest body )
  `(let
     ((key-map  (make-sparse-keymap)))

     ,@(mapcar
         (lambda ( key-fn-pair )
           `(define-key key-map
              ,(eval (car key-fn-pair))

              ,(if (symbol-function (cdr key-fn-pair))
                 `',(cdr key-fn-pair)
                 (cdr key-fn-pair)) ) )
         body)

     (define-key key-map "h" (keybindings-help-local ,description key-map))

     (,(if global
         'global-set-key
         'local-set-key)
       (kbd (concat custom-key-group-prefix " " ,chord)) key-map)

     (custom-key-group-register ,chord ,description key-map) ))

(defmacro custom-key-set ( description chord global &rest body )
  `(let
     ((key-map  (make-sparse-keymap)))

     ;; do this part just so we can create help
     ,@(mapcar
         (lambda ( key-fn-pair )
           `(keymap-set key-map
              ,(kbd (car key-fn-pair))

              ,(if (symbol-function (cdr key-fn-pair))
                 `',(cdr key-fn-pair)
                 (cdr key-fn-pair)) ) )
         body)

     (custom-key-group-register ,chord ,description key-map)

     ;;
     ;; unbind everything
     ;;

     ,@(mapcar
         (lambda ( key-fn-pair )
           `(global-unset-key (kbd ,(concat chord "-" (car key-fn-pair)) ) ) )
         body)

     ,@(mapcar
         (lambda ( key-fn-pair )
           `(local-unset-key (kbd ,(concat chord "-" (car key-fn-pair)) ) ) )
         body)

     ,@(mapcar
         (lambda ( key-fn-pair )
           `(,(if global
                'global-set-key
                'local-set-key)

              (kbd ,(concat chord "-" (car key-fn-pair)) )

              ,(if (symbol-function (cdr key-fn-pair))
                 `',(cdr key-fn-pair)
                 (cdr key-fn-pair)) ) )
         body) ))

(custom-key-group "help" "h" t
  ("a" . apropos)
  ("v" . describe-variable)
  ("f" . describe-function)
  ("k" . describe-key)
  ("g" . keybindings-help-global))

(provide 'custom-key)
