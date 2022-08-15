# Grail configuration for Emacs

An Emacs configuration and loader.

## Early History

Grail began as a Emacs lisp cut and paste mess of config snippets
around 2000 or so. I swore that one day I would learn Elisp so I could
change the file without breaking it.

It was copied around as a .emacs file and then one day I sat down to
learn Emacs and wrote a proper config and eventually checked it into
SubVersion.

After a lot of hacking for a while I migrated to git and used it as a
local repository.

## Big re-design

When I moved to the big city and started a job that required more
power in the Emacs department to compensate for the poor tools. I
rewrote it into it's current design in three months of round the clock
overtime.

The design was to "bootstrap" it's dependencies by simply dropping in
the config, making a symlink, and setting an environment variable
pointing to the config.

When it loads it traps all the loading errors from missing modules and
maps them to installers that download and build all the third party
modules needed by the config.

It can mostly load on the first try, and usually works well after a
reboot.

## Modern rewrite

In August of 2022 the original code was over-complicated, unreliable,
and was unecessary since everything is hosted on git. Being able to
load from files, tarballs, cvs, hg, and others was just impossible to
maintain bloat.

I junked the deployment model in favor of stripping down to only the
bare necessities, and pulling dependencies with git submodules. I
rewrote the loader and the profile mechanism completely simplifying
the code and fixing bugs.

## Fix self location

There was one still glaring problem is that grail didn't really
know how to find itself. grail.el is symlinked to ~/.emacs but the
code needs to know the root of the repo to load.

Under Linux this was accomplished with an environment variable but
both Linux Display Managers and MacOS dock launched Apps are
extremely difficult to set per-user environment variables for a
login session.

Finally I came up with a second symlink: $HOME/.emacs.grail that points
to the repo. Grail loads from that symlink now.


