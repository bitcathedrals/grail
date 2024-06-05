;; -*-no-byte-compile: t; -*-

(require 'ucase-word)
(require 'buffer-status)

(require 'whitespace-utilities)
(require 'eol-utilities)

(defun scrub-all ()
  (interactive)
  (if (buffer-modifiable-p)
    (progn
      (scrub-dos-eol)
      (scrub-tabs))
    (message "cannot scrub in a read-only buffer!") ))

(defun print-hex ( number )
  "print the hex of a number, faster than firing up calc mode"
  (message "the hex is %x" number))

(defun rid-window ()
  "get rid of the current window"
  (interactive)
  (delete-windows-on (current-buffer)))

(defun insert-key-notation ()
  "inject a complete \(kbd \"sequence\"\) with key notation for a key sequence given by prompt"
  (interactive)
  (insert "(kbd \"")
  (insert (format-kbd-macro (read-key-sequence "Key? " nil t)))
  (insert "\")"))

(defun visit-url ( url )
  "visit a url in a new buffer"

  ;; it would be cooler if the default was the last item from the clipboard.
  (interactive "sURL? ")
  (progn
    (switch-to-buffer (generate-new-buffer url))
    (url-insert-file-contents url)))

(defun buffer-to-clipboard ()
  "copy the entire buffer to the clipboard"
  (interactive)
  (mark-whole-buffer)
  (copy-region-to-clipboard))

(defun show-call ( fn )
  "show-call

   Call a function printing the return value of the function as a message.
   This is really handy for seeing what a function does in the current
   buffer.
  "
  (interactive "afunction? ")
  (message "returned: %s" (princ (funcall fn))) )

(defun what-face (pos)
  (interactive "d")
  (let ((face (or (get-char-property (point) 'read-face-name)
                  (get-char-property (point) 'face))))
    (if face (message "Face: %s" face) (message "No face at %d" pos))))

(defun copy-buffer ( &rest args )
  (interactive)

  (save-excursion
    (let
      ((end   (progn
                (end-of-buffer)
                (point)))
       (start (progn
                (beginning-of-buffer)
                (point))))

      (copy-region-as-kill start end) )))

(defun copy-region-to-clipboard ()
  "copy the region to the clipboard"
  (interactive)
  (gui-select-text (filter-buffer-substring (region-beginning) (region-end))) )

(provide 'user-commands)
