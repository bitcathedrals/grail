#! /usr/bin/env bash

TOOLS=$HOME/tools
PYENV_ROOT="$TOOLS/pyenv"
PATH="$TOOLS/local/bin:$PATH"
PATH="$PYENV_ROOT/bin:$PATH"
PATH="$PYENV_ROOT/libexec:$PATH"

export PYENV_ROOT PATH

exec pyenv virtualenvs | tr -s ' ' | cut -d ' ' -f 2 | grep -v -E '^\d'
