#! /usr/bin/env bash

TARGET=$HOME/.emacs.d/tree-sitter

test -d $TARGET || mdkir -p $TARGET

system=$(uname)

GRAIL_ROOT="$(dirname $0)/.."
GRAIL_TREE_SITTER=$(readlink -f "$GRAIL_ROOT/emacs/dist/tree-sitter/")

echo "tree-sitter repos in: $GRAIL_TREE_SITTER"

SHARED_OBJECTS="so"

case $system in
  "Darwin")
     eval "$(/opt/emacs/bin/brew shellenv)"

     SHARED_OBJECTS="dylib"
   ;;
esac

for lang in $(find $GRAIL_TREE_SITTER -type d -depth 1 -print)
do
  echo "installing language: $lang"

  (cd $lang && make clean && make && cp *.${SHARED_OBJECTS} $TARGET)
done
