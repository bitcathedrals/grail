EMACSCLIENT=`which emacsclient`

$EMACSCLIENT -e "(ext-diff \"$1\" \"$2\")"

