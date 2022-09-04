# Grail for Emacs

An Emacs configuration and loader.

## Early History

Grail began as a Emacs lisp cut and paste mess of snippets from
Googling around 2000 or so. I swore that one day I would learn Elisp so I could change the file without breaking it.

I copied around as a .emacs file from machine to machine and then one day I sat down to learn Elisp and wrote a proper config. I checked it into Subversion and started maintaining it properly.

After a lot of hacking on it I migrated it to git and used several third party modules to extend the basic functionality.


## Big re-design

When I moved to the big city and started a job I required more power from the Emacs to compensate for the poor tools on the job site. I rewrote it into a toolset designed to run in a VM and built a layer over Perforce using RCS that gave me local version control and diffing on top of perforce, and advanced SSH usage.

I rewrote the core of the configuration, the loader, to "bootstrap" or deploy it's dependencies by simply dropping in the config, making a symlink, and setting an environment variable pointing to the config.

When it loaded it would trap all the loading errors from missing modules and map them to installers that download and installed all the missing third party modules needed by the config.

It mostly worked on the the first try and just needed to be run a couple of times to install and load everything. At the time the Version Control and package landscape was highly diverse so it supported svn, git, hg, tarballs, and others I can't remember.

It stayed in this state up through 2014 when I stopped using Emacs as my primary development environment.

## Modern rewrite

In August of 2022 the original code was over-complicated, unreliable, and was unnecessary since everything is hosted on git. Being able to load from files, tarballs, cvs, hg, and others was just impossible to maintain bloat.

I junked the deployment capabilities in favor of stripping down to only the bare necessities, and pulling dependencies with git
submodules. Everything is git these days.

I rewrote the loader and the profile mechanism vastly simplifying the code and fixing bugs. 

The current form of the loader is just concerned with the profile mechanism, setting up load-path, and setting up the display. I removed the --daemon functionality and simplified the frame setup accordingly.

This is the current evolution of the config.

## Fix self location

There was still one glaring problem is that grail didn't really know how to find itself. grail.el is symlinked to ~/.emacs but the code needs to know the root of the repo to load.

Under Linux this was accomplished with an environment variable but both Linux Display Managers and the MacOS dock launcher make setting per-user environment variables for a login session extremely difficult.

Finally I came up with a second symlink: $HOME/.emacs.grail that points to the repo. Grail loads from that symlink now. This neatly solves the problem in a platform independent way.

## Status

It currently works on console, and graphical environments. It is portable between MacOS, Linux, and FreeBSD.

## Installation

First you need to clone the git repository. The 'main' branch is kept in a stable state so you should clone that initially.

```bash
git clone https://github.com/bitcathedrals/grail.git emacs-grail
```

Second you need to cd into emacs-grail and you need to initialize the dependencies. One dependency will be my pythonsh toolbox for running py.sh commands, the rest are Elisp packages.

The install.sh script executes the submodule command for you:

```bash
cd emacs-grail
scripts/install.sh
```

Third you need to create the symlinks that point the Emacs load at grail:

```bash
ln -s <INSTALL DIR>/emacs-grail/emacs/grail.el ~/.emacs
ln -s <INSTALL_DIR>/emacs-grail/emacs/ ~/.emacs.grail
```

This should be enough to start grail on the console or in a GUI environment.

Within emacs-grail you should take note of emacs/systems/:
- macos.el
- freebsd.el
- windows.el
- linux.el

Modify these as necessary if your system needs tweaking.

You can create a directory "emacs/hosts/<hostname>.el" and that will be loaded on that host.

You can create d a directory "emacs/users/<username>.el" and that file will be loaded for that user.

## Basic Use

There are many key sequences already constructed. They are all on the prefix C-c, which is the user defined prefix. The best
place to start is C-c h g which will show global keybindings.
Press "q" to exit the help buffer.

Note: the keybinding C-h is remapped to backspace as on the freeBSD console the backspace key generates a C-h. Please use C-c h f for describe-function, C-c h v for describe-variable, and C-c h k for describe-key.

For most languages C-c e e will eval the line, C-c e d will eval a define. These are in the help.

For all of the key mappings defined with my custom-key system invoking C-c <group> h will bring up a description of the chord.

