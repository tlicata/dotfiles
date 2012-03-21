This is my emacs configuration tree.

To use it:

0) Install emacs.

To get emacs24, I did:

    sudo add-apt-repository ppa:cassou/emacs
    sudo apt-get update
    sudo apt-get install emacs-snapshot

1) Clone this repository to ~/.emacs.d. If ~/.emacs.d already exists then back it up first.

    git clone <repo_url> ~/.emacs.d

2) The repo contains submodules that will be empty until they are inited and updated.

    cd ~/.emacs.d
    git submodule init
    git submodule update

3) Start emacs

Optional:

4) Undo-tree will be slow unless byte compiled.  M-x byte-compile-file <ENTER> undo-tree.el <ENTER>

5) Make Caps Lock act as another Ctrl key.

(On Ubuntu) -> System Menu -> Preferences -> Keyboard -> Layouts tab -> Options button ->
Ctrl key position -> Make CapsLock an additional Ctrl.

6) If terminal has a menu bar then Menu Access keys can interfere with emacs META commands.  For
instance, M-f will open the File menu instead of moving forward a word.

In terminal menu bar -> Edit -> Keyboard Shortcuts -> Uncheck "Enable menu access keys".

7) EMMS relies on external media players.  On Ubuntu, installing mplayer allowed
me to player mp3s.  Otherwise I got "Don't know how to play track" errors.

    sudo apt-get install mplayer


Notes:

* M-x query-replace RET C-q C-M RET RET (gets rid of "^M"s in code)