#! /usr/bin/env bash

shopt -s lastpipe

IN_FILE=$1

if [[ -n $IN_FILE -a -f $IN_FILE ]]
then
  echo "compiling input file: ${IN_FILE}..."
else
  echo "ERROR: NO input file: ${IN_FILE}"
  exit 1
fi

OUT_FILE=$2

cat <<ELISP | tr -s "\n" " " | read CODE
(with-temp-buffer
 (insert-file \\"${IN_FILE}\\")

 (org-mode)
 (org-babel-tangle)

 (message \\"compiled: ${FILE} -> %s\\" (get-inspiration)) )
ELISP

if [[ $3 == "--show" ]]
then
  echo "code is: $CODE"
fi

exec emacsclient -e "$CODE"
