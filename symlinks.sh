#!/bin/bash

readonly dotfiles=".bash_aliases .emacs.d .gitconfig .tmux.conf"
readonly dotfilesdir="$(cd "$(dirname "$0")" && pwd)"
readonly timestamp="$(date +%s)"

forEachFile () {
    for filename in $dotfiles
    do
        $1 $filename
    done
}

removeSymlink () {
    existing=$HOME/$1
    if [ -h $existing ]
    then
        echo "removing symbolic link $existing"
        rm $existing
    fi
}

backupExisting () {
    existing=$HOME/$1
    if [ -e $existing ]
    then
        backup=$existing.$timestamp
        echo "backing up $existing to $backup"
        mv $existing $backup
    fi
}

createSymlink () {
    existing=$HOME/$1
    current=$dotfilesdir/$1
    echo "symlinking $current to $existing"
    ln -s $current $existing
}

if [ "$1" == "-d" ]
then
    forEachFile removeSymlink
else
    forEachFile removeSymlink
    forEachFile backupExisting
    forEachFile createSymlink
fi
