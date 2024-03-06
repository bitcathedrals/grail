#! /usr/bin/env bash
test -d ~/code || mkdir ~/code
git clone https://github.com/bitcathedrals/grail.git ~/code/grail

(cd ~/code/grail && git submodule update --init)

ln -s ~/code/emacs/grail.el ~/.emacs
ln -s ~/code/emacs/ ~/.emacs.grail

cd ~/code/grail/emacs/dist/git/slime && make compile-swank
