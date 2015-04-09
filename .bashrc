# move /usr/local/bin to front of PATH
export PATH=/usr/local/bin:$PATH

export PS1="\W \$ "
export CLICOLOR=1

# export JAVA_TOOL_OPTIONS=-Dfile.encoding=UTF-8

### Added by the Heroku Toolbelt
export PATH="/usr/local/heroku/bin:$PATH"

### Add rbenv
export PATH="$HOME/.rbenv/bin:$PATH"
eval "$(rbenv init -)"
