#! /usr/bin/env bash

app=""

user_brew=/Users/mattie/homebrew/Cellar/emacs-plus@29/29.2/Emacs.app
opt_brew=/usr/local/opt/emacs-plus@29/Emacs.app

if [[ -d $user_brew ]]
then
   app=$user_brew
   echo "using homedir brew: $user_brew"
else
  if [[ -d $opt_brew ]]
  then
    app=$opt_brew
  else
    echo "brew app could not be found in $user_brew or $opt_brew"
    exit 1
  fi
fi

osascript -e "tell application \"Finder\" to make alias file to posix file \"$app\" at POSIX file \"/Applications\""
