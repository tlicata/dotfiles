#!/bin/bash

dotfiles=".bash_aliases .emacs.d .gitconfig .tmux.conf"
dotfilesdir="$(cd "$(dirname "$0")" && pwd)"
timestamp="$(date +%s)"

for filename in $dotfiles
do
    existing=$HOME/$filename
    current=$dotfilesdir/$filename
    if [ -e $existing ]
    then
        if [ -h $existing ]
        then
            echo "not backing up symbolic link $existing"
            rm $existing
        else
            backup=$existing.$timestamp
            echo "backing up $existing to $backup"
            mv $existing $backup
        fi
    fi
    echo "symlinking $current to $existing"
    ln -s $current $existing
done
