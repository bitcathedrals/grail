(require 'oc)
(require 'oc-bibtex)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)
   (ruby . t)
   (shell . t)
   (scheme . t)
   (lisp . t)
   (java . t)) )

(defun org/mk-markdown ()
  "org/mk-markdown

   generate markdown. file will have same name, different extension.
  "
  (interactive)
  (org-md-export-to-markdown))


(defun org/mk-code ()
  "org/mk-code

   generate the source code from the org file
  "
  (interactive)
  (org-babel-tangle))


(defun org/mk-pdf ()
  "org/mk-code

   export to pdf
  "
  (interactive)
  (org-latex-export-to-pdf))

(defun tangle-non-interactive (file)
  (with-current-buffer (find-file-read-only file)
    (org-babel-tangle)

    (message "compiled: %s -> %s" file (get-inspiration)) ))


