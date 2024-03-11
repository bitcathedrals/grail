#! /usr/bin/env bash

case $1 in
  "compile")
    doas apt install -y build-essential libssl-dev zlib1g-dev libbz2-dev libreadline-dev libsqlite3-dev wget curl llvm libncurses5-dev libncursesw5-dev xz-utils tk-dev libffi-dev liblzma-dev

    GIT=$HOME/code/emacs
    test -d $GIT || mkdir -p $GIT
    test -d $GIT/.git || git clone https://github.com/emacs-mirror/emacs.git $GIT

    command -v autoconf >/dev/null 2>&1
    if [[ $? -ne 0 ]]
    then
      echo >/dev/stderr "autoconf is required to build emacs - please install autoconf."
      exit 1
    fi

    command -v automake >/dev/null 2>&1
    if [[ $? -ne 0 ]]
    then
      echo >/dev/stderr "automake is required to build emacs - please install automake."
      exit 1
    fi

    command -v makeinfo >/dev/null 2>&1

    if [[ $? -ne 0 ]]
    then
      echo >/dev/stderr "makeinfo is required to build emacs - please install texinfo."
      exit 1
    fi

    command -v gcc >/dev/null 2>&1
    if [[ $? -ne 0 ]]
    then
      echo >/dev/stderr "gcc is required to build emacs - please install gcc."
      exit 1
    fi

    TOOLS=$HOME/tools/local/
    test -d $TOOLS || mkdir -p $TOOLS

    (cd $GIT && ./autogen.sh && ./configure --prefix=$TOOLS --with-x-toolkit=gtk3 --with-native-compilation=yes --with-xpm=no --with-gif=no && make && make install)

    LOCAL_DESKTOP=$HOME/.local/share/applications/
    test -d $LOCAL_DESKTOP || mkdir -p $LOCAL_DESKTOP

    cp desktop/emacs.desktop $LOCAL_DESKTOP/
    cp desktop/emacs-icon.png $HOME/tools/
  ;;
  case "macos")
    brew tap d12frosted/emacs-plus
    brew install emacs-plus --with-native-comp || exit 1

    brew unlink emacs && brew link emacs-plus@29 || exit 1

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
  ;;

  *|"help")
    cat <<HELP
comile-emacs.sh

macos  = compile emacs from homebrew source and install into /Applications
linux  = compile emacs for linux and install into ~/tools/local
HELP
  ;;
esac
