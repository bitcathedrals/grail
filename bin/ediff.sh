#! /usr/bin/env bash

case $1 in
  "^")
    shift
    exec git difftool $@
  ;;
  "+")
    shift
    exec git mergetool $@
  ;;
  *|"help")

cat <<HELP
ediff.sh - interface for using ediff with git

^ git diff   (wrap git difftool)
+ git merge  (wrap git mergetool)

for files use run-emacs directly.
HELP

esac

exit 0

