#! /usr/bin/env bash

VENV="emacs"

eval "$(pyenv init -)"
pyenv virtualenvs | grep $VENV >/dev/null 2>&1 || pyenv activate $VENV

pyenv exec pip install --upgrade pip 
exec pyenv exec python3 -m pip install python-lsp-server

