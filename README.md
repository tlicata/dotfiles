# Install emacs

To get emacs24:

(on Ubuntu 12.xx)

    sudo add-apt-repository ppa:cassou/emacs
    sudo apt-get update
    sudo apt-get install emacs-snapshot

(on Ubuntu 13.xx)

    sudo apt-get install emacs24

(on Mac OS X)

    brew install emacs

# Clone repository

    git clone <repo_url> ~/dotfiles

# Set up symlinks

    ~/dotfiles/symlinks.sh

Passing the -d option will delete them.

    ~/dotfiles/symlinks.sh -d

# Optional

* If terminal has a menu bar then Menu Access keys can interfere with
  emacs META commands.  For instance, M-f will open the File menu
  instead of moving forward a word.

In terminal menu bar -> Edit -> Keyboard Shortcuts -> Uncheck "Enable menu access keys".

## Notes

* Emacs24 was throwing errors of "variable is void: collection"

https://github.com/technomancy/emacs-starter-kit/issues/142  
https://bugs.launchpad.net/emacs-snapshot/+bug/1003928

Workaround is

    $ cd /usr/share/emacs/24.1.50/etc
    $ sudo ln -s DOC-24.1.50.2 DOC-24.1.50.1
