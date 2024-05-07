;; -*-no-byte-compile: t; -*-

(require 'vc-git)

(which-function-mode)

(defun get-battery-buffer ()
  (get-buffer-create "*battery output*"))

(defun get-battery-buffer-for-output ()
  (let
    ((battery-buffer (get-battery-buffer)))

    (with-current-buffer battery-buffer
      (erase-buffer))

    battery-buffer))

(defun mega-modeline-battery-apm ()
  (call-process "apm" nil (get-battery-buffer-for-output) nil "-l")

  (with-current-buffer (get-battery-buffer)
    (let
      ((percentage (extract-string-with-regex "[0-9]+")))

      (when percentage
        (setq mega-modeline-battery-level (concat percentage "%"))) )) )

(defun mega-modeline-battery-pmset ()
  (call-process "pmset" nil (get-battery-buffer-for-output) nil "-g" "batt")

  (with-current-buffer (get-battery-buffer)
    (let
      ((percentage (extract-string-with-regex "[0-9]+%")))

      (when percentage
        (setq mega-modeline-battery-level percentage)) )) )

(defun mega-modeline-battery-acpi ()
  (call-process "acpi" nil (get-battery-buffer-for-output) nil)

  (with-current-buffer (get-battery-buffer)
    (let
      ((percentage (extract-string-with-regex "[0-9]+%")))

      (when percentage
        (setq mega-modeline-battery-level percentage)) )) )

(defun mega-modeline-battery-dummy ()
  nil)

(defvar mega-modeline-battery-command 'mega-modeline-battery-dummy)

(defvar mega-modeline-battery-level "")

(defun mega-modeline-find-battery-command ()
  (mapcar
    (lambda (prog)
      (when (executable-find (car prog))
        (setq mega-modeline-battery-command (cdr prog))) )

    '(("apm" . mega-modeline-battery-apm)
      ("pmset" . mega-modeline-battery-pmset)
      ("acpi" . mega-modeline-battery-acpi)) ))

(defun mega-modeline-update-battery-level ()
  (funcall mega-modeline-battery-command))

(defconst mega-modeline-battery-update-minutes 5)
(defconst mega-modeline-battery-update-seconds (* mega-modeline-battery-update-minutes
                                                    60))

(defun mega-modeline-battery-set-timer ()
  (run-with-timer 0 mega-modeline-battery-update-seconds 'mega-modeline-update-battery-level))

;;----------------------------------------------------------------------
;; mega-modeline
;;----------------------------------------------------------------------
(defun mega-modeline-branch-null ()
  "")

(defvar mega-modeline-branch 'mega-modeline-branch-null)
(defvar mega-modeline-battery "?")

(defun mega-modeline-modified ()
  "mega-modeline-modified

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
    (if (and (boundp 'mega-modeline-vcs) (functionp mega-modeline-vcs))
      (funcall mega-modeline-vcs)
      "")
    "]"))

(defun setup-mega-modeline ()
  (setq-default mode-line-format
    '(" "
      "[" (:eval (buffer-name)) "]"

      " "

      (line-number-mode "%l")
      ">"
      (column-number-mode "%c")

      " "

      (:eval (mega-modeline-modified))

      " "

      "[" (:propertize mode-name face (:weight bold)) "] "

      buffer-ring-modeline

       " <"

       mega-modeline-battery-level

       "> /"

       (:eval (car (vc-git-branches)))
       ))

  (mega-modeline-find-battery-command)
  (mega-modeline-battery-set-timer)

  (mega-modeline-update-battery-level)
  (force-mode-line-update) )

(provide 'mega-modeline)
