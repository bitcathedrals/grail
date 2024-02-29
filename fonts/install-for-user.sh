#! /usr/bin/env bash

test -e spleen || tar xf spleen.tar

cp spleen/*.dfont ~/Library/Fonts/
cp hack/*.ttf ~/Library/Fonts
cp dejavu-sans-mono/*.ttf ~/Library/Fonts


