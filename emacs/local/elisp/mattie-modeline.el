;; -*-no-byte-compile: t; -*-

(which-function-mode)

(defun get-battery-buffer ()
  (get-buffer-create "*battery output*"))

(defun get-battery-buffer-for-output ()
  (let
    ((battery-buffer (get-battery-buffer)))

    (with-current-buffer battery-buffer
      (erase-buffer))

    battery-buffer))

(defun mattie-modeline-battery-apm ()
  (call-process "apm" nil (get-battery-buffer-for-output) nil "-l")

  (with-current-buffer (get-battery-buffer)
    (let
      ((percentage (extract-string-with-regex "[0-9]+")))

      (when percentage
        (setq mattie-modeline-battery-level (concat percentage "%"))) )) )

(defun mattie-modeline-battery-pmset ()
  (call-process "pmset" nil (get-battery-buffer-for-output) nil "-g" "batt")

  (with-current-buffer (get-battery-buffer)
    (let
      ((percentage (extract-string-with-regex "[0-9]+%")))

      (when percentage
        (setq mattie-modeline-battery-level percentage)) )) )

(defun mattie-modeline-battery-acpi ()
  (call-process "acpi" nil (get-battery-buffer-for-output) nil)

  (with-current-buffer (get-battery-buffer)
    (let
      ((percentage (extract-string-with-regex "[0-9]+%")))

      (when percentage
        (setq mattie-modeline-battery-level percentage)) )) )

(defun mattie-modeline-battery-dummy ()
  nil)

(defvar mattie-modeline-battery-command 'mattie-modeline-battery-dummy)

(defvar mattie-modeline-battery-level "")

(defun mattie-modeline-find-battery-command ()
  (mapcar
    (lambda (prog)
      (when (executable-find (car prog))
        (setq mattie-modeline-battery-command (cdr prog))) )

    '(("apm" . mattie-modeline-battery-apm)
      ("pmset" . mattie-modeline-battery-pmset)
      ("acpi" . mattie-modeline-battery-acpi)) ))

(defun mattie-modeline-update-battery-level ()
  (funcall mattie-modeline-battery-command))

(defconst mattie-modeline-battery-update-minutes 5)
(defconst mattie-modeline-battery-update-seconds (* mattie-modeline-battery-update-minutes
                                                    60))

(defun mattie-modeline-battery-set-timer ()
  (run-with-timer 0 mattie-modeline-battery-update-seconds 'mattie-modeline-update-battery-level))

;;----------------------------------------------------------------------
;; mattie-modeline
;;----------------------------------------------------------------------
(defun mattie-modeline-branch-null ()
  "")

(defvar mattie-modeline-branch 'mattie-modeline-branch-null)
(defvar mattie-modeline-battery "?")

(defun mattie-modeline-modified ()
  "mattie-modeline-modified

   Construct a modified string for the modeline.
  "
  (concat
    "[" (symbol-name buffer-file-coding-system) "]"
    " "

    "["
    (if (or (and (local-variable-p 'view-read-only) view-read-only)
          (and (local-variable-p 'buffer-read-only) buffer-read-only))
      "RO"
      "RW")
    "/"
    (cond
      ((not (verify-visited-file-modtime (current-buffer))) "!DSK")
      ((buffer-modified-p)  "!CHG")
      ((and buffer-file-name (recent-auto-save-p)) "!BKP")
      (t "-"))
    "/"
    (if (and (boundp 'mattie-modeline-vcs) (functionp mattie-modeline-vcs))
      (funcall mattie-modeline-vcs)
      "")
    "]"))

(defun setup-mattie-modeline ()
  (setq-default mode-line-format
    '(" "
      "[" (:eval (buffer-name)) "]"

      " "

      (line-number-mode "%l")
      ">"
      (column-number-mode "%c")

      " "

      (:eval (mattie-modeline-modified))

      " "

      "[" (:propertize mode-name face (:weight bold)) "] "

      buffer-ring-modeline

       " <"

       mattie-modeline-battery-level

       "> /"

       (:eval (car (vc-git-branches)))
       ))

  (mattie-modeline-find-battery-command)
  (mattie-modeline-battery-set-timer)

  (mattie-modeline-update-battery-level)
  (force-mode-line-update) )

(provide 'mattie-modeline)
