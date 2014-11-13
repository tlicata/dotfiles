if [ -f ~/.bash_aliases ]; then
    source ~/.bash_aliases
fi

export PATH=/usr/local/bin:$PATH

export PS1="\W \$ "
