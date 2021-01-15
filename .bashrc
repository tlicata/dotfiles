# move /usr/local/bin to front of PATH
export PATH=/usr/local/bin:$PATH

export PS1="\W \$ "
export CLICOLOR=1

# Add Erlang build tool, rebar3, to path.
export PATH=/home/tim/.cache/rebar3/bin:$PATH
# Erlang flags recommened by Adopting Erlang.
# https://adoptingerlang.org/docs/development/setup/
export ERL_AFLAGS="+pc unicode -kernel shell_history enabled"
