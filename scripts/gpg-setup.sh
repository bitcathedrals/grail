#! /usr/bin/env bash

system=$(uname -a | cut -d ' ' -f 1)

agent_conf=$HOME/.gnupg/gpg-agent.conf

if [[ -f $agent_conf ]]
then
  echo "gpg-setup.sh: agent config $agent_conf already exists, aborting"
  exit 1
fi

case $system in
  "Darwin")
    echo "gpg-setup.sh: setting up for Darwin"

    pinentry=`which pinentry-mac`

    if [[ $? -ne 0 ]]
    then
      brew install pinentry-mac

      pinentry=`which pinentry-mac`
    fi

    echo >>$agent_conf "pinentry-program $pinentry"
  ;;
  "FreeBSD")
    echo "gpg-setup.sh: setting up for FreeBSD"

    cat >>$agent_conf <<CONFIG
allow-emacs-pinentry
allow-loopback-pinentry
CONFIG
  ;;
  *)
    echo "unrecognized system: $system, aborting"
    exit 1
  ;;
esac



