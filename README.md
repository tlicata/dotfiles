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

