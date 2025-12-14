#! /usr/bin/env bash

case $1 in
  "^")
    echo >/dev/stderr "ediff.sh: diff against upstream"
    exec git difftool "@{u}..HEAD"
  ;;
  "^^")
    echo >/dev/stderr "ediff.sh: diff3 against upstream"
    exec git difftool "@{u}...HEAD"
  ;;
  "@")
    shift
    left=$1

    shift
    right=$1

    echo >/dev/stderr "ediff.sh: diff (not in both) ${left} ${right}"
    exec git difftool "${left}..${right}"
  ;;
  "@@")
    shift
    left=$1

    shift
    right=$1

    echo >/dev/stderr "ediff.sh: diff3 (not in both) ${left} ${right}"
    exec git difftool "${left}...${right}"
  ;;
  "+")
    shift
    exec git mergetool $@
  ;;
  *)
   exec git difftool $@
  ;;
  "help")
cat <<HELP
ediff.sh - interface for using ediff with git

(working+staged) =  git diff staged

^   (upstream)      = diff  against upstream
^^  (upstream)      = diff3 against upstream

@   <left> <right>  = diff  left against right revision
@@  <left> <right>  = diff3 left against right revision

+ merge <args>      = merge with arguments

for files use run-emacs directly.
HELP
esac
