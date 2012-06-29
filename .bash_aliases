if type emacs-snapshot > /dev/null 2>&1
then
        alias emacs="emacs-snapshot -nw"
else
        alias emacs="emacs -nw"
fi

alias ll="ls -l -h"
alias la="ls -a"
alias l="ls"
alias lla="ls -a -l"
alias lh="ls -l -h"
