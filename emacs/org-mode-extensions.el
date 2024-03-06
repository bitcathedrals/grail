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


