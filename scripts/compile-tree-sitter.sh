#! /usr/bin/env bash

INSTALL_TO=$HOME/.emacs.d/tree-sitter

LANG=$1

test -d $INSTALL_TO || mkdir -p $INSTALL_TO

cd emacs/dist/tree-sitter/$LANG

make

cp *.so *.dylib $INSTALL_TO/
