#! /usr/bin/env bash

IN_FILE="$PWD/$1"

if [[ -n $IN_FILE && -f $IN_FILE ]]
then
  echo "compiling input file: ${IN_FILE}..."
else
  echo "ERROR: NO input file: ${IN_FILE}"
  exit 1
fi

exec $EDITOR -e "(tangle-non-interactive \"$IN_FILE\")"
