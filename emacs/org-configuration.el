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

(setq org-latex-compiler "xelatex")
(setq org-latex-pdf-process
      (list (concat "latexmk -"
                    org-latex-compiler 
                    " -recorder -synctex=1 -bibtex-cond %b")))

(setq org-latex-listings t)

(setq org-latex-default-packages-alist
      '(("" "graphicx" t)
        ("" "grffile" t)
        ("" "longtable" nil)
        ("" "wrapfig" nil)
        ("" "rotating" nil)
        ("normalem" "ulem" t)
        ("" "amsmath" t)
        ("" "textcomp" t)
        ("" "amssymb" t)
        ("" "capt-of" nil)
        ("" "hyperref" nil)))

;; I don't know how this works, pray.

(setq org-latex-classes
'(("article"
"\\RequirePackage{fix-cm}
\\PassOptionsToPackage{svgnames}{xcolor}
\\documentclass[11pt]{article}
\\usepackage{fontspec}
\\setmainfont{ETBembo RomanOSF}
\\setsansfont[Scale=MatchLowercase]{Raleway}
\\setmonofont[Scale=MatchLowercase]{Operator Mono SSm}
\\usepackage{sectsty}
\\allsectionsfont{\\sffamily}
\\usepackage{enumitem}
\\setlist[description]{style=unboxed,font=\\sffamily\\bfseries}
\\usepackage{listings}
\\lstset{frame=single,aboveskip=1em,
framesep=.5em,backgroundcolor=\\color{AliceBlue},
rulecolor=\\color{LightSteelBlue},framerule=1pt}
\\usepackage{xcolor}
\\newcommand\\basicdefault[1]{\\scriptsize\\color{Black}\\ttfamily#1}
\\lstset{basicstyle=\\basicdefault{\\spaceskip1em}}
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
\\usepackage[a4paper,margin=1in,left=1.5in]{geometry}
\\usepackage{parskip}
\\makeatletter
\\renewcommand{\\maketitle}{%
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
\\AtBeginDocument{\\renewcommand{\\UrlFont}{\\ttfamily}}
[PACKAGES]
[EXTRA]"
("\\section{%s}" . "\\section*{%s}")
("\\subsection{%s}" . "\\subsection*{%s}")
("\\subsubsection{%s}" . "\\subsubsection*{%s}")
("\\paragraph{%s}" . "\\paragraph*{%s}")
("\\subparagraph{%s}" . "\\subparagraph*{%s}"))

("report" "\\documentclass[11pt]{report}"
("\\part{%s}" . "\\part*{%s}")
("\\chapter{%s}" . "\\chapter*{%s}")
("\\section{%s}" . "\\section*{%s}")
("\\subsection{%s}" . "\\subsection*{%s}")
("\\subsubsection{%s}" . "\\subsubsection*{%s}"))

("book" "\\documentclass[11pt]{book}"
("\\part{%s}" . "\\part*{%s}")
("\\chapter{%s}" . "\\chapter*{%s}")
("\\section{%s}" . "\\section*{%s}")
("\\subsection{%s}" . "\\subsection*{%s}")
("\\subsubsection{%s}" . "\\subsubsection*{%s}"))))

(defun org/cite ()
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


