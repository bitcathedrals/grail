#! /usr/bin/env bash

case $1 in
  "^")
   shift
   exec git difftool $@
  ;;
  "^^")
    exec git difftool "--" "staged"
  ;;
  "*")
    exec git difftool "@{u}..HEAD"
  ;;
  "**")
    shift
    left=$1

    shift
    right=$1

    echo >/dev/stderr "ediff.sh: merge diff (not in both) ${left} ${right}"
    exec git difftool "${left}..${right}"
  ;;
  "+")
    shift
    exec git mergetool $@
  ;;
  *|"help")

cat <<HELP
ediff.sh - interface for using ediff with git

^  (working+staged) = git diff (args?)
^^ (staged)         = git diff staged

*  (track)          = against upstream
** (merge) <a> <b>  = diff what's not in <a> or <b>

+ merge <args>      = merge with arguments

for files use run-emacs directly.
HELP

esac

exit 0

