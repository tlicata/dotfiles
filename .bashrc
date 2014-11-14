if [ -f ~/.bash_aliases ]; then
    source ~/.bash_aliases
fi

# move /usr/local/bin to front of PATH
export PATH=/usr/local/bin:$PATH

export PS1="\W \$ "
export CLICOLOR=1
