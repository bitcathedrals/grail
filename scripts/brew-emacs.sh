#! /usr/bin/env bash

brew tap d12frosted/emacs-plus
brew install emacs-plus --with-native-comp

brew unlink emacs
brew link emacs-plus@29
