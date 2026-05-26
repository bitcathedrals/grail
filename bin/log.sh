#! /usr/bin/env bash

case $1 in
  "^")
    echo >/dev/stderr "log.sh: log against upstream"
    exec git log "@{u}..HEAD"
  ;;
  "@")
    shift
    left=$1

    shift
    right=$1

    echo >/dev/stderr "log.sh: log (not in both) ${left} ${right}"
    exec git log "${left}...${right}"
  ;;
  "help")
cat <<HELP
log.sh - interface for using advanced git log commands easily

^   (upstream)      = log current against upstream
@   <left> <right>  = log not in left and right revisions
HELP
  ;;
  *)
   exec git log $@
  ;;
esac
