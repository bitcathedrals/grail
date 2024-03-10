#! /usr/bin/env bash

app=""

home_brew=$(find /Users/mattie/homebrew -name 'Emacs.app' -print)
user_brew=$(find /usr/local/opt/ -name 'Emacs.app' -print)
opt_brew=$(find /opt/homebrew/ -name 'Emacs.app' -print)

if [[ -e $home_brew ]]
then
   app=$home_brew
fi

if [[ -e $user_brew ]]
then
  app=$user_brew
fi

if [[ -e $opt_brew ]]
then
  app=$opt_brew
fi


if [[ -z $app ]]
then
  echo "brew app could not be found in $home_brew or $user_brew or $opt_brew"
  exit 1
fi

osascript -e "tell application \"Finder\" to make alias file to posix file \"$app\" at POSIX file \"/Applications\""
