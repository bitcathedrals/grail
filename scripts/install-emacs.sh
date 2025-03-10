#! /usr/bin/env bash

GIT=$HOME/code/emacs

NATIVE=no
SITTER=yes
# ifavailable
TLS=yes
MODULES=yes
MAIL=yes
PNG=yes

case $1 in
  "linux")
    TOOLS=$HOME/tools/local/

doas apt install -y build-essential libssl-dev zlib1g-dev libbz2-dev libreadline-dev \
libsqlite3-dev wget curl llvm libncurses5-dev libncursesw5-dev xz-utils \
libffi-dev liblzma-dev libtree-sitter-dev libgnutls28-dev autoconf texinfo \
libgtk-3-dev

    test -d $GIT || git clone https://git.savannah.gnu.org/git/emacs.git $GIT

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

    if (cd $GIT && make extraclean && \
          ./autogen.sh && \
          ./configure \
            --prefix=$TOOLS \
            --with-x-toolkit=gtk3 \
            --with-mailutils=$MAIL \
            --with-native-compilation=$NATIVE \
            --with-tree-sitter=$SITTER \
            --with-xpm=no \
            --with-png=$PNG \
            --with-gif=no \
            --with-gnutls=$TLS \
            --with-modules=$MODULES \
            --without-webp && \
          make bootstrap)
    then
      echo "compile ok."
    else
      echo "COMPILE FAILED!"
      exit 1
    fi

    read -p "Proceed with emacs install? [y/n]: " proceed

    if [[ $proceed = "y" ]]
    then
      echo ">>> proceeding with install!"
    else
      echo ">>> ABORT! exiting now!"
      exit 1
    fi

    test -d $TOOLS || mkdir -p $TOOLS

    (cd $GIT && make install)

    LOCAL_DESKTOP=$HOME/.local/share/applications/
    test -d $LOCAL_DESKTOP || mkdir -p $LOCAL_DESKTOP

    sed <desktop/emacs.desktop "s,@HOME@,$HOME,g" >$LOCAL_DESKTOP/emacs.desktop
    cp desktop/emacs-icon.png $HOME/tools/
  ;;
  "macos-arm64")
    test -d /opt/emacs || sudo mkdir -p /opt/emacs
    curl -L https://github.com/Homebrew/brew/tarball/master >/tmp/brew.xz
    sudo tar xJf /tmp/brew.xz --strip 1 -C /opt/emacs/
    sudo chown -R mattie /opt/emacs
    ;;
  "macos-deps")
    eval "$(/opt/emacs/bin/brew shellenv)" && \
      arch -arm64 brew install autoconf automake texinfo nettle rust gnutls pkg-config libpng tree-sitter little-cms2
    ;;
   "macos-git")
    TOOLS=$HOME/tools/local/

    test -d $GIT || git clone https://git.savannah.gnu.org/git/emacs.git $GIT

    eval "$(/opt/emacs/bin/brew shellenv)"

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

    if (cd $GIT && eval "$(/opt/emacs/bin/brew shellenv)" && \
          arch -arm64 make extraclean && \
          ./autogen.sh && \
          arch -arm64 ./configure \
                         --prefix=$TOOLS \
                         --with-ns \
                         --with-mailutils=$MAIL \
                         --with-native-compilation=$NATIVE \
                         --with-tree-sitter=$SITTER \
                         --with-xpm=no \
                         --with-png=$PNG \
                         --with-gif=no \
                         --with-gnutls=$TLS \
                         --with-modules=$MODULES \
                         --without-webp && \
          arch -arm64 make bootstrap)
    then
      echo "compile ok."
    else
      echo "COMPILE FAILED!"
      exit 1
    fi

    read -p "Proceed with emacs install? [y/n]: " proceed

    if [[ $proceed = "y" ]]
    then
      echo ">>> proceeding with install!"
    else
      echo ">>> ABORT! exiting now!"
      exit 1
    fi

    test -d $TOOLS || mkdir -p $TOOLS

    (cd $GIT && make install)
  ;;
  "macos-compile")
    brew tap d12frosted/emacs-plus
    brew install emacs-plus@29 || exit 1
  ;;
  "macos-link")
    brew_emacs="emacs-plus@29"

    brew unlink $brew_emacs
    brew link $brew_emacs

    if [[ $? -ne 0 ]]
    then
      echo /dev/stderr "install-emacs.sh: could not link $brew_emacs"
      exit 1
    fi

    app=$(brew list $brew_emacs | grep Emacs.app | sed -e 's,/Contents/.*$,,g' | sort -u)

    if [[ -z $app ]]
    then
      echo "brew app could not be found in $brew_emacs listing!"
      exit 1
    fi

    osascript -e "tell application \"Finder\" to make alias file to posix file \"$app\" at POSIX file \"/Applications\""
  ;;
  *|"help")
    cat <<HELP
comile-emacs.sh

macos-compile  = compile emacs from homebrew source and install into /Applications
macos-link     = link the app into /Applications
linux          = compile emacs for linux and install into ~/tools/local
HELP
  ;;
esac
