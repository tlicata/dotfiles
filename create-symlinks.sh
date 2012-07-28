#!/bin/bash

readonly dotfiles=".bash_aliases .emacs.d .gitconfig .tmux.conf"
readonly dotfilesdir="$(cd "$(dirname "$0")" && pwd)"
readonly timestamp="$(date +%s)"

removeSymlinks () {
    for filename in $dotfiles
    do
        existing=$HOME/$filename
        if [ -h $existing ]
        then
            echo "removing symbolic link $existing"
            rm $existing
        fi
    done
}

backupExisting () {
    for filename in $dotfiles
    do
        existing=$HOME/$filename
        if [ -e $existing ]
        then
            backup=$existing.$timestamp
            echo "backing up $existing to $backup"
            mv $existing $backup
        fi
    done
}

createSymlinks () {
    for filename in $dotfiles
    do
        existing=$HOME/$filename
        current=$dotfilesdir/$filename
        echo "symlinking $current to $existing"
        ln -s $current $existing
    done
}

if [ "$1" == "-d" ]
then
    removeSymlinks
else
    removeSymlinks
    backupExisting
    createSymlinks
fi
