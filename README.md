# Install emacs

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

Sometimes the meta key does not behave properly.

* On Ubuntu, if terminal has a menu bar then Menu Access keys can
  interfere with Emacs meta commands.  For instance, M-f will open the
  File menu instead of moving forward a word.

  In terminal menu bar -> Edit -> Keyboard Shortcuts -> Uncheck
  "Enable menu access keys".

* On Mac OS X,

  Terminal -> Preferences -> Keyboard -> Check "Use option as meta key"

