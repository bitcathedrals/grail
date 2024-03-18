;;
;; org-mode configuration
;;

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
  (list (concat (getenv "HOME") "/code/compsci/bibliography.bib")
        (concat (getenv "HOME") "/code/compsci/philosophy.bib")) )

(setq
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
    ("" "hyperref" nil)
    ("backend=biber" "biblatex" t)))

(defun latex-core (type org &rest latex-cells)
  (append
    (list
      type
      (concat "
\\documentclass[12pt]{" type "}
\\usepackage[a4paper,margin=.5in,left=.5in]{geometry}
\\RequirePackage{fix-cm}
\\PassOptionsToPackage{svgnames}{xcolor}
\\usepackage{fontspec}
\\usepackage{sectsty}
\\usepackage{enumitem}
\\setlist[description]{style=unboxed,font=\\sffamily\\bfseries}
\\usepackage{listings}
\\usepackage{xcolor}
\\usepackage{fancyhdr}
\\usepackage{lastpage}
\\usepackage{parskip}
\\usepackage[nodayofweek]{datetime}
\\usepackage{titling}

\\pretitle{\\begin{center}\\Large\\bfseries}
\\posttitle{\\par\\end{center}}
\\preauthor{\\begin{center}\\Large}
\\postauthor{\\end{center}}
\\predate{\\begin{center}}
\\postdate{\\end{center}}

\\newdateformat{mydate}{\\twodigit{\\THEDAY}{ }\\shortmonthname[\\THEMONTH], \\THEYEAR}

\\pagestyle{fancy}

\\fancyhf{}
\\pagenumbering{arabic}

\\setlength{\\marginparwidth}{1pt}

\\renewcommand{\\headrulewidth}{0pt}
\\renewcommand{\\footrulewidth}{1pt}

\\lfoot{\\today}
\\cfoot{\\thepage \\hspace{1pt}/\\pageref{LastPage}}
\\rfoot{" org "}

\\newcommand\\basicdefault[1]{\\scriptsize\\color{Black}\\ttfamily#1}
\\lstset{basicstyle=\\basicdefault{\\spaceskip1em}}
\\lstset{frame=single,aboveskip=1em, framesep=.5em,backgroundcolor=\\color{AliceBlue}, rulecolor=\\color{LightSteelBlue},framerule=1pt}
\\lstset{literate=
    {§}{{\\S}}1
    {©}{{\\raisebox{.125ex}{\\copyright}\\enspace}}1
    {«}{{\\guillemotleft}}1
    {»}{{\\guillemotright}}1
    {Á}{{\\'A}}1
    {Ä}{{\\\"A}}1
    {É}{{\\'E}}1
    {Í}{{\\'I}}1
    {Ó}{{\\'O}}1
    {Ö}{{\\\"O}}1
    {Ú}{{\\'U}}1
    {Ü}{{\\\"U}}1
    {ß}{{\\ss}}2
    {à}{{\\`a}}1
    {á}{{\\'a}}1
    {ä}{{\\\"a}}1
    {é}{{\\'e}}1
    {í}{{\\'i}}1
    {ó}{{\\'o}}1
    {ö}{{\\\"o}}1
    {ú}{{\\'u}}1
    {ü}{{\\\"u}}1
    {¹}{{\\textsuperscript1}}1
            {²}{{\\textsuperscript2}}1
            {³}{{\\textsuperscript3}}1
    {ı}{{\\i}}1
    {—}{{---}}1
    {’}{{'}}1
    {…}{{\\dots}}1
            {⮠}{{$\\hookleftarrow$}}1
    {␣}{{\\textvisiblespace}}1,
    keywordstyle=\\color{DarkGreen}\\bfseries,
    identifierstyle=\\color{DarkRed},
    commentstyle=\\color{Gray}\\upshape,
    stringstyle=\\color{DarkBlue}\\upshape,
    emphstyle=\\color{Chocolate}\\upshape,
    showstringspaces=false,
    columns=fullflexible,
    keepspaces=true}
\\makeatletter
\\renewcommand{\\maketitle}{
  \\begingroup\\parindent0pt
  \\sffamily
  \\Huge{\\bfseries\\@title}\\par\\bigskip
  \\LARGE{\\bfseries\\@author}\\par\\medskip
  \\normalsize\\@date\\par\\bigskip
  \\endgroup\\@afterindentfalse\\@afterheading}
\\makeatother
[DEFAULT-PACKAGES]
\\hypersetup{linkcolor=Blue,urlcolor=DarkBlue,
  citecolor=DarkRed,colorlinks=true}
\\AtBeginDocument{\\renewcommand{\\UrlFont}{\\ttfamily}}"))

    latex-cells))

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
      (latex-core "book" "Michael Mattie"
        ("\\part{%s}" . "\\part*{%s}")
        ("\\chapter{%s}" . "\\chapter*{%s}")
        ("\\section{%s}" . "\\section*{%s}")
        ("\\subsection{%s}" . "\\subsection*{%s}")
        ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))

      (latex-core "article" "Michael Mattie"
        ("\\part{%s}" . "\\part*{%s}")
        ("\\chapter{%s}" . "\\chapter*{%s}")
        ("\\section{%s}" . "\\section*{%s}")
        ("\\subsection{%s}" . "\\subsection*{%s}")
        ("\\subsubsection{%s}" . "\\subsubsection*{%s}")) )) )

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

(defun org/mk-pdf (use)
  "org/mk-code

   export to pdf
  "
  (interactive (list (completing-read "use for? " '("work" "personal"))))

  (when (string-equal use "work")
    (latex-for-work))

  (when (string-equal use "personal")
    (latex-for-personal))

  (message "compiling document for: %s" use)
;;  (org/mk-clean)
  (org-latex-export-to-pdf) )

(defun org/mk-clean ()
  "org/mk-clean

  clean the intermediary files"
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
  (with-current-buffer (find-file-read-only file)
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
    ("b" . helm-bibtex)) )

(add-hook 'org-mode-hook 'org-mode-customize)

(provide 'profile/org-mode)
