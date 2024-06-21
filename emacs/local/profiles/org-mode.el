;;
;; org-mode configuration
;;

(require 'ebib)

(bibtex-set-dialect 'biblatex)

(defconst latex-output "Latex Output" t)
(defconst latex-errors "Latex Errors" t)

(require 'custom-key)

(setq
  org-ref-completion-library 'org-ref-insert-cite-link)

(setq bibtex-autokey-year-length 4
      bibtex-autokey-name-year-separator "-"
      bibtex-autokey-year-title-separator "-"
      bibtex-autokey-titleword-separator "-"
      bibtex-autokey-titlewords 2
      bibtex-autokey-titlewords-stretch 1
      bibtex-autokey-titleword-length 5)

;; (setq
;;   org-ref-completion-library 'org-ref-helm-cite
;;   org-ref-get-pdf-filename-function 'org-ref-get-pdf-filename-helm-bibtex)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)
   (ruby . t)
   (shell . t)
   (scheme . t)
   (lisp . t)
   (java . t)) )

(defconst org-latex-compiler "xelatex")

(setq org-latex-pdf-process (list "latexmk -pdfxe -bibtex-cond -g -gg -recorder %f"))

(setq org-cite-export-processors '((latex biblatex) ))

(setq
  org-latex-listings t
  org-use-sub-superscripts nil
  org-export-with-sub-superscripts nil
  org-cite-global-bibliography
  (list
    (concat (getenv "HOME") "/code/compsci/bibliography.bib")
    (concat (getenv "HOME") "/code/personal/philosophy.bib")
    (concat (getenv "HOME") "/code/personal/economics.bib")
    (concat (getenv "HOME") "/code/personal/reference.bib")
    (concat (getenv "HOME") "/code/personal/fiction.bib")
    (concat (getenv "HOME") "/code/personal/scifi.bib")
    (concat (getenv "HOME") "/code/personal/biography.bib"))

  bibtex-completion-bibliography org-cite-global-bibliography
  org-cite-follow-processor 'helm-bibtex-org-cite-follow)

(defconst org-latex-default-packages-alist
  '(("" "inputenc" nil)
    ("" "graphicx" t)
    ("" "grffile" t)
    ("" "longtable" nil)
    ("" "wrapfig" nil)
    ("" "rotating" nil)
    ("normalem" "ulem" t)
    ("" "amsmath" t)
    ("" "amssymb" t)
    ("" "capt-of" nil)
    ("hidelinks" "hyperref" nil)
    ("backend=biber" "biblatex" t)))

(defconst profile/org-latex-dir (concat grail-local-dir "/latex/") "the grail latex directory")

(defun profile/org-load-common (type org)
  (with-temp-buffer
    (insert-file-contents (concat profile/org-latex-dir "common.tex"))

    (replace-string "@TYPE@" type)
    (replace-string "@ORG@" org)

    (buffer-substring (point-min) (point-max))))

(defun latex-core (type org &rest sections)
  (append
    (list
      type
      (profile/org-load-common type org))
    sections))

(defun latex-for-work ()
  "latex-for-gauge-security

  configure latex for company use"
  (interactive)

  (setq org-latex-classes
    (list
      (latex-core "book" "Gauge Security LLC"
        '("\\part{%s}" . "\\part*{%s}")
        '("\\chapter{%s}" . "\\chapter*{%s}")
        '("\\section{%s}" . "\\section*{%s}")
        '("\\subsection{%s}" . "\\subsection*{%s}")
        '("\\subsubsection{%s}" . "\\subsubsection*{%s}"))

      (latex-core "report" "Gauge Security LLC"
        '("\\chapter{%s}" . "\\chapter*{%s}")
        '("\\section{%s}" . "\\section*{%s}")
        '("\\subsection{%s}" . "\\subsection*{%s}")
        '("\\subsubsection{%s}" . "\\subsubsection*{%s}"))

      (latex-core "article" "Gauge Security LLC"
        '("\\section{%s}" . "\\section*{%s}")
        '("\\subsection{%s}" . "\\subsection*{%s}")
        '("\\subsubsection{%s}" . "\\subsubsection*{%s}")) )) )

(defun latex-for-personal ()
  (interactive)
  "latex-for-gauge-security

   configure latex for personal use
  "
  (setq org-latex-classes
    (list
      (latex-core "book" "Mr. Mattie"
        '("\\part{%s}" . "\\part*{%s}")
        '("\\chapter{%s}" . "\\chapter*{%s}")
        '("\\section{%s}" . "\\section*{%s}")
        '("\\subsection{%s}" . "\\subsection*{%s}")
        '("\\subsubsection{%s}" . "\\subsubsection*{%s}"))

      (latex-core "article" "Mr. Mattie"
        '("\\section{%s}" . "\\section*{%s}")
        '("\\subsection{%s}" . "\\subsection*{%s}")
        '("\\subsubsection{%s}" . "\\subsubsection*{%s}")) )) )

(defun org/cite ()
  "org/cite

  insert a citation"
  (interactive)
  (org-cite-insert))

(defun org/mk-markdown ()
  "org/mk-markdown

   generate markdown. file will have same name, different extension.
  "
  (interactive)
  (org-md-export-to-markdown))

(defun org/mk-code ()
  "org/mk-code

   tangle: generate the source code from the org file
  "
  (interactive)
  (org-babel-tangle))

(defun org/mk-html ()
  "org/mk-html

   export to html
  "
  (interactive)
  (org-html-export-to-html))

(defun org/mk-pdf (use)
  "org/mk-odf

   export to pdf
  "
  (interactive (list (completing-read
                       "org?: " ;; prompt
                       '("work" "personal") ;; completions
                       nil     ;; predicate
                       t       ;; require match
                       nil     ;; initial input
                       nil     ;; history
                       "work"  ;; default value
                       nil)))  ;; inherit input method

  (cond
    ((string-equal use "work") (latex-for-work))
    ((string-equal use "personal") (latex-for-personal)) )

  (message "compiling document for: %s" use)
;;  (org/mk-clean)
  (org-latex-export-to-pdf))

(defun org/mk-clean ()
  "org/mk-clean

  clean all tmp files"
  (interactive)

  (let
    ((default-directory (file-name-directory buffer-file-name)))
    (shell-command "latexmk -c && rm *.tex *.bbl *.log")) )

(defun org/mk-pristine ()
  "org/pristine

   clean all temp files and all output files."
  (interactive)

  (org-mk-clean)

  (shell-command "latexmk -C"
    (get-buffer-create latex-output t)
    (get-buffer-create latex-errors t)) )

(defun tangle-non-interactive (file)
  "tangle-non-interactive
   command to generate code designed for emacsclient eval.
   "
  (with-current-buffer (find-file file)
    (org-babel-tangle)

    (message "compiled: %s -> %s" file (get-inspiration)) ))

(defun org-mode-customize ()
  (interactive)

  (custom-key-group "org" "o" nil
    ("t"  . org/mk-code)
    ("p" . org/mk-pdf)
    ("i" . org-ref-insert-cite-link)
    ("c" . org/mk-clean)
    ("C" . org/mk-pristine)
    ("m" . org-ref-bibtex-hydra/body) ;; main menu
    ("H" . org/mk-html)
    ("b" . helm-bibtex)) )

(add-hook 'org-mode-hook 'org-mode-customize)
(add-hook 'org-mode-hook 'flyspell-mode-on)

(provide 'profile/org-mode)
