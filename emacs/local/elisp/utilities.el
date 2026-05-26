;;----------------------------------------------------------------------
;; lisp utilities
;;----------------------------------------------------------------------
(defun lisp-map ( fn list )
  (let
    ((transform (funcall fn (car list))))

    (if (not (eq nil (cdr list)))
      (cons transform (lisp-map fn (cdr list)))
      (cons transform (cdr list)) )))

(provide 'utilities)
;;----------------------------------------------------------------------
;; elisp.el
;;----------------------------------------------------------------------
(require 'async-exec)

;; make sure that the pretty printer doesn't truncate which frustrates my
;; development.

(setq
 print-length nil
 eval-expression-print-level nil
 print-level nil)

(defun strip-list-last ( list )
  "strip the last element from a list"
  (if (consp (cdr list))
    (cons
      (car list)
      (strip-list-last (cdr list)))
    nil))

(defun run-hooks-with-arg ( hook-list &rest args )
  "run the hook list with arg"
  (mapc
    (lambda ( hook )
      (apply hook args))
      hook-list))

(defun todays-date ()
  "return today's date as a string."
  (let
    ((now (decode-time))
     (string nil))

    (mapc (lambda ( x )
            (setq string
              (if string
                (concat (number-to-string x) "-" string)
                (number-to-string x))))
      (list (nth 3 (decode-time)) (nth 4 (decode-time)) (nth 5 (decode-time)) ))
    string))

(defun other-window-forward ()
  (interactive)
  (other-window 1))

(defun select-word ()
  (interactive)
  (let
    ((begin nil)
     (end   nil)
     (pos   (point)))

    (catch 'fail
      (save-excursion
        (backward-word)
        (setq begin (point))

        (goto-char pos)

        (forward-word)
        (setq end (point))

        (unless (and (< begin pos )
                     (> end pos ))
          (throw 'fail nil)) )

      (push-mark begin)
      (goto-char end)
      t)))

(defun run-custom-hooks ( hook-list )
  (mapc
    (lambda ( hook )
      (funcall hook))
    hook-list))
