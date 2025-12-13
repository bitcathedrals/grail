;;
;; tests for creating a merge file name from the local
;;

(grail-diff-is-arg-ok nil)

(grail-diff-is-arg-ok t)

(grail-diff-is-arg-ok "")

(grail-diff-is-arg-ok "ok")

(grail-diff-check-diff-args nil nil)

(grail-diff-check-diff-args "test ok" ni)

(grail-diff-check-diff-args nil "test ok")

(grail-diff-check-diff-args "" "test ok")

(grail-diff-check-diff-args "test ok" "")

(grail-diff-check-diff-args "test ok" "test ok")



;; Expected output is a filename, plus a hour and minute time in military time, and extension.
;; The purpose is to name a file that is temporary, so that the merged file can be edited
;; and reviewed without damaging a source file.

;; If the merge looks good the file can be written to the proper file with C-x w

(grail-diff-merge-file-name "~/test/foo.c" "~/test/bar.c")

(grail-diff-merge-file-name "file.c" "bar.c")

(grail-diff-merge-file-name "/home/file.c" "/home/bar.c")

;;
;; create a merge buffer from the merge filename
;;

(grail-diff-get-merge-buffer "file.c" "bar.c")

;;
;; start a diff session from two files
;;

(let
  ((default-directory "~/code/grail/tests/ediff/"))

  (grail-diff-elisp "local.c" "upstream.c"))

;;
;; test auto refine
;;

(let
  ((default-directory "~/code/grail/tests/ediff/"))

  (grail-diff-elisp "local.c" "refine.c"))

;;
;; test with ancestor for diff
;;

(let
  ((default-directory "~/code/grail/tests/ediff/"))

  (grail-diff-ancestor-elisp "local.c" "upstream.c" "ancestor.c"))

(let
  ((default-directory "~/code/grail/tests/ediff/"))

  (grail-diff-merge-elisp "local.c" "upstream.c"))

(let
  ((default-directory "~/code/grail/tests/ediff/"))

  (grail-diff-merge-ancestor-elisp "local.c" "upstream.c" "ancestor.c"))
