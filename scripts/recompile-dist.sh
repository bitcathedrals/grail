#! /usr/bin/env bash

if [[ ! -d emacs ]]
then
  echo "recompile-dist.sh: need to be in the root of the grail repository"
  exit 1
fi

find emacs/dist/git -name '*.elc' -print | xargs rm

if [[ -x emacsclient ]]
then
  CLIENT=emacsclient
else  
  CLIENT=$EDITOR
fi

eval $CLIENT -e '"(compile-dist)"'
