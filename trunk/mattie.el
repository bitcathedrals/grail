;;----------------------------------------------------------------------
;; mattie.el
;; Primary Author: Mike Mattie
;; Copyright: Mike Mattie (2007)
;; License: GPL v3.
;;----------------------------------------------------------------------

(defun file-if-readable ( file )
  "this function was created because file-readable-p is strangely akward in that it returns t
   instead of the path it was given which neccesitates this silly wrapper. Consider sending
   this upstream as a patch or add-on to file-readable-p"

  (if (file-readable-p file)
    file))

(defun print-hex ( number )
  "print the hex of a number, faster than firing up calc mode"
  (message "the hex is %x" number))

(defun show-bad-ws()
  (interactive)
  (highlight-regexp "\t"))

(defun rid-window ()
  "get rid of the current window"
  (interactive)
  (delete-windows-on (current-buffer)))

(defun insert-key-notation ()
  "inject a complete \(kbd \"sequence\"\) with key notation for a key sequence given by prompt"
  (interactive)
  (insert "(kbd \"")
  (insert (format-kbd-macro (read-key-sequence "Key? " nil t)))
  (insert "\")")
  )

(defun visit-url ( url )
  "visit a url in a new buffer"
  (interactive "sURL? ")
  (progn
    (switch-to-buffer (generate-new-buffer url))
    (url-insert-file-contents url)))

;;----------------------------------------------------------------------
;; bounds-scan. Scan for the bounds indicated by delimiters. This
;; is aimed at code so allow ignored nested delimiters.
;;
;; The goal of this hack is to see if I can grow the sophistication
;; of the implementation without compromising the simplicity of the
;;
;; BUG: this doesn't handle quoted delimiters, which should not be that
;;      hard.
;;----------------------------------------------------------------------

(defun bounds-scan ( position level )
  "scan for the delimitation of a region. This is the core algorithm
   that counts opening and closing delimiters to track the nesting
   of delimiters. The algorithm terminates when a closing delimiter
   is encountered on level 0."
  (progn
    (goto-char position) ;; move to the starting position before scanning.
    (funcall seek-bounds)

    (cond
      ((funcall open-bound-p)
        (bounds-scan (funcall restart-position) (+ level 1)))

      ((funcall close-bound-p)
        (if (> level 0)
          ;; when we have a positive level to start with
          ;; scan again with a decremented level.
          (bounds-scan (funcall restart-position) (- level 1))

          ;; return point as we are done
          (point)
          ))
      ;; error ?
      )))

(defmacro match-char-property ( name &rest p )
  "Match against char properties with the form: \(property regex...\).
   If any of the regex's match against the property value non-nil is
   returned, nil otherwise."

  `(lexical-let ((property (get-text-property (point) ',name)))
       (if property
         (or
           ,@(mapcar (lambda (m) `(string-match ,m (symbol-name property))) p)
           ))
       ))

(defun symbol-for-direction ( direction forward backward )
  "little helper function to choose a symbol based on direction
   given. I can bury some error handling later"
  (cond
    ((string-equal "next" direction) forward)
    ((string-equal "prev" direction) backward))
  )

(defmacro skip-over-properties ( iterator &rest predicates )
  "skip-over-properties starts from the point, searching for a position before
   the given delimiters. To ignore text at a syntantic level such as comments
   and literals where the delimiters may be embedded, properties of text to
   skip over can be given."
  (lexical-let
    ((direction (car iterator))
      (delimiters (cadr iterator)))

    `(lexical-let
       ;; bind the iterator as a lambda so we can eval more than once.
       ((iter (lambda ()
                ,(list
                   ;; select the correct skip-chars based on direction
                   (symbol-for-direction direction 'skip-chars-forward 'skip-chars-backward)

                   ;; construct the regex given to skip-chars-, all we need to add is
                   ;; the inversion since skip-chars wraps given regex  in a []
                   `(concat "^" ,delimiters))
                ))

         ;; we need to bound the iterator which a special predicate. The iterator always stops
         ;; before the delimiter it seeks, so we need a predicate that uses char-{before,after}
         (iter-stop-p (lambda ()
                        (string-match
                          (concat "[" ,delimiters "]")
                          (char-to-string ( ,(symbol-for-direction direction 'char-after 'char-before) ))
                            )))

         ;; determine if where we have stopped is blacklisted by virtue of it's
         ;; text properties.
         (prop-skip-p (lambda ()
                        (or
                          ;; each attribute that can be examined goes inside the or
                          ;; so that multiple attributes can be examined.

                          ;; the predicate for a specific property are built with mach-char-property.
                          ,@(mapcar (lambda (p) `(match-char-property ,@p)) predicates)
                          )))
         )

     (while (cond
       ;; first check if the character properties at the point are blacklisted
       ;; with the prop-p predicate, scan past those properties.
       ((while (funcall prop-skip-p)
          (goto-char ( ,(symbol-for-direction direction '+ '-) (point) 1))) t)
       ;; first bound the iterator with iter-stop-p, otherwise search as necessary.
       ((and (not (funcall iter-stop-p)) (funcall iter)) t)
     )))
    ))

;; (defun my-test ()
;;  "foo"
;;  (interactive)
;;  (if (skip-over-properties (next "\(\)") (face ".*comment.*" ".*string.*" ".*doc.*"))
;;    (message "it worked !")
;;    (message "it failed !")
;;    ))

(defun bounds-scan-forward ( delimiters position )
  "entry point for bounds-scan forward. given delimiters: a
   string containing a pair of delimiting characters, which must
   be in \"open close\" order, scan forward for the bounding
   delimiter returning the position before the delimiter"

  (lexical-let
    ((delimiter-re (concat "^" delimiters))
      (open-delimiter (aref delimiters 0))
      (close-delimiter (aref delimiters 1)))

    (let*
      ((open-bound-p     (lambda ()
                            (char-equal open-delimiter (char-after))))
        (close-bound-p    (lambda ()
                            (char-equal close-delimiter (char-after))))
        (seek-bounds      (lambda ()
                            (skip-over-properties (next delimiters) (face
                                                                      ".*comment.*"
                                                                      ".*string.*"
                                                                      ".*doc.*"))))
        (restart-position (lambda ()
                            (+ (point) 1)))

        (close-at (bounds-scan position 0)))
      ;; Keep the returned position within the delimiters if the
      ;; scan moved beyond the starting position.
      (if (> close-at position)
        (- close-at 1)
        position)
      )))

(defun bounds-scan-backward ( delimiters position )
  "entry point for bounds-scan backward. given delimiters: a
   string containing a pair of delimiting characters, which must
   be in \"open close\" order, scan backward for the bounding
   delimiter returning the position after the delimiter"

  (lexical-let
    ;; note the inversion of the order since we are looking backwards
    ((delimiter-re (concat "^" delimiters))
      (open-delimiter (aref delimiters 1))
      (close-delimiter (aref delimiters 0)))

    (let*
      ((open-bound-p     (lambda ()
                           (char-equal open-delimiter (char-before))))
        (close-bound-p    (lambda ()
                            (char-equal close-delimiter (char-before))))
        (seek-bounds      (lambda ()
                            (skip-over-properties (prev delimiters) (face
                                                                      ".*comment.*"
                                                                      ".*string.*"
                                                                      ".*doc.*"))))

        (restart-position (lambda ()
                            (- (point) 1)))

        (open-at (bounds-scan position 0)))

      ;; Keep the returned position within the delimiters if the
      ;; scan moved beyond the starting position.
      (if (< open-at position)
        (+ open-at 1)
        position)
      )))

(defun scan-lisp-list-close ()
  "wrapper for bounds-scan that searches for the closing delimiter of a lisp list"
  (bounds-scan-forward "()" (point)))

(defun scan-lisp-list-open ()
  "wrapper for bounds-scan that searches for the opening delimiter of a lisp list"
  (bounds-scan-backward "()" (point)))

(defun lisp-list-delete-body ()
  "delete the body of a lisp list including any nested lists"
  (interactive)
  (let
    ((open-pos (scan-lisp-list-open))
     (close-pos (scan-lisp-list-close)))

    (delete-backward-char (- close-pos open-pos))))

(defun xml-before-doc-close ()
  "move the point immediately before the closing of the document"
  (interactive)
  (end-of-buffer)
  (re-search-backward "</" nil t))

;;----------------------------------------------------------------------
;; repl
;;
;; Handy tool for exploratory programming. pops a new frame with the
;; interpreter for the language running in a REPL loop.
;;----------------------------------------------------------------------

(defun repl ( lang )
  "start a REPL interpreter interface in a new frame based upon a
  given or inferred language parameter"

  (interactive "MLanguage: ")

  (lexical-let
    ((repl-frame (make-frame))
      )

    (select-frame-set-input-focus repl-frame)

    (cond
      ((string-equal "perl5" lang)
	(switch-to-buffer (make-comint "perl5 REPL" "/usr/bin/perl" nil "-d" "-e shell")))
      ((string-equal "elisp" lang)
        (ielm))
      (else
        (message "I don't support language %s" lang)))

  (add-hook 'kill-buffer-hook
    (lambda ()
      (delete-frame repl-frame))
    t t)
    ))

(defun examine-library (library-name)
  "examine the source of a library. Type the library name without
   any extension. If the library exists the source will be
   loaded"

  (interactive "F")
  (find-file-read-only (locate-library (concat library-name ".el")))
  )

;; This is a handy little function that allows you to localize
;; a distributed elisp source file. It assumes that the current
;; buffer is a distributed elisp file, and that localized-source-dir
;; points to a real directory.

(defun localize-distrib ()
  "localize a distributed lisp file by writing a copy of the file
   to a directory searched before the distributed lisp files"
  (interactive)

  (let
    ((new-name (file-name-nondirectory (buffer-file-name))))

    (let
      ((new-path
        (concat localized-source-dir
          (if (string-equal "gz" (file-name-extension new-name))
            (file-name-sans-extension new-name)
            (new-name)))))
      (if (yes-or-no-p (concat "localize distributed file " new-name " to " new-path))
        (write-file new-path)
        (message "aborted localizing distributed file"))
    )))

;; required for my patched em-unix, note: merged upstream, may collide
;; on a update.
(defun nil-blank-string ( string )
  "if a string is all blanks return nil, if there are non-blank characters return the string"
  (if (string-match "[^[:blank:]]" string ) string))

;;----------------------------------------------------------------------
;; local function library.
;;----------------------------------------------------------------------

;; taken from the Lisp Intro text as rendered by:
;; http://www.rattlesnake.com/intro/print_002delements_002dof_002dlist.html

;; modified slightly to issue debugging bread-crumbs to the Messages buffer
(defun debug-elements-of-list (list)
  "Print each element of LIST on a line of its own."
  (while list
    (message "debug: element %s" (car list))
    (setq list (cdr list))))

(defun merge-changes ()
  "Merge latest changes against the last checkout."
  (interactive)
  (let
    ( (merge-file (buffer-file-name))
      (wc-file (concat (buffer-file-name) ".merge"))
      )

    (save-excursion
      (let
        ((wc-buffer (progn
                      (write-file wc-file)
                      (buffer-name)))

          ;; using vc-workfile-version is necessary so that subsequent merges
          ;; get the correct head-buffer
          (head-buffer (vc-find-version merge-file (vc-workfile-version merge-file)))

          (merge-buffer (progn
                          (vc-revert-file merge-file)
                          (find-file merge-file)))
        )

        ;; ? check for an exit status from ediff
        (ediff-merge-buffers head-buffer wc-buffer nil nil merge-file)

        ;; ? make sure the changes were saved
        ))
      ))

