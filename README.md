# Install emacs

To get emacs24, I did:

(on Ubuntu)

    sudo add-apt-repository ppa:cassou/emacs
    sudo apt-get update
    sudo apt-get install emacs-snapshot

(on Mac OS X)

* Download a .dmg from http://emacsformacosx.com/builds
* Double click it
* Select option to copy it to Applications folder

# Clone repository

    git clone <repo_url> ~/dotfiles

# Set up symlinks

    ~/dotfiles/create-symlinks.sh

# Optional

* Make Caps Lock act as another Ctrl key.

(On Ubuntu) -> System Menu -> Preferences -> Keyboard -> Layouts tab -> Options button ->
Ctrl key position -> Make CapsLock an additional Ctrl.

* If terminal has a menu bar then Menu Access keys can interfere with
  emacs META commands.  For instance, M-f will open the File menu
  instead of moving forward a word.

In terminal menu bar -> Edit -> Keyboard Shortcuts -> Uncheck "Enable menu access keys".

## Notes

* M-x query-replace RET C-q C-M RET RET (gets rid of "^M"s in code)

* Emacs24 was throwing errors of "variable is void: collection"

https://github.com/technomancy/emacs-starter-kit/issues/142  
https://bugs.launchpad.net/emacs-snapshot/+bug/1003928

Workaround is

    $ cd /usr/share/emacs/24.1.50/etc
    $ sudo ln -s DOC-24.1.50.2 DOC-24.1.50.1
