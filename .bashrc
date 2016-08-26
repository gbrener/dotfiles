#export LD_LIBRARY_PATH=/usr/local/pgsql/lib
export VISUAL="emacsclient -c -s /tmp/emacs1000/server -a ''"
export EDITOR="$VISUAL"
export LESS="-MQR" # Enable long-prompt, quiet-operation, and ANSI colors in "less"
export HISTCONTROL=ignoreboth
export HISTIGNORE='[bf]g:exit:history:history *'
if [ `uname` = Linux ]; then
    export HISTSIZE=1000
else
    export HISTSIZE=10
fi
export MOZ_USE_OMTC=1 # Enable hardware acceleration in Firefox
export PYTHONSTARTUP="$HOME/.pythonrc.py"

export PATH="$HOME/anaconda/bin:/usr/local/bin:$PATH"

set -o ignoreeof # Prevent accidental logouts when hitting C-d
set -o notify # Notify me asynchronously when background jobs finish
#set -o nolog # Don't log history
shopt -s -q cdspell # Spell-check paths
if [ ${BASH_VERSINFO[0]} -ge 4 ]; then
    shopt -s -q direxpand # Expand directories (>= v4)
    shopt -s -q dirspell # Spell-check directories (>= v4) 
    shopt -s -q globstar # Support the ** glob pattern (>= v4)
fi
shopt -s -q cmdhist # Save multi-line cmds
shopt -s -q lithist # Store multi-line cmds with newlines instead of ;
shopt -s -q checkwinsize # Update rows/columns as necessary after each cmd

alias emacs=$VISUAL
alias rm='\rm -iv'
alias cp='\cp -iv'
alias mv='\mv -iv'
alias gdb='\gdb --quiet'
alias hist='history | less'
alias l='\ls -1F'
alias ns='netstat -xaupen'
if [ `uname` = Linux ]; then
    alias ls='\ls -AFghv --time-style=long-iso'
else
    alias ls='\ls -AFgh'
fi
alias install_pkg='makepkg -sri'

function timer_start {
  timer=${timer:-$SECONDS}
}

function timer_stop {
  timer_show=$(($SECONDS - $timer))
  unset timer
}

# Call timer_start at the beginning of commands and scripts
set -o functrace
trap 'timer_start' DEBUG
export PROMPT_COMMAND=timer_stop
PROMPT_COLOR=32 # green
if [ $TERM = xterm ]; then
    UPDATE_XTERM_TITLE='\e]0;$HOSTNAME\007'
else
    unset UPDATE_XTERM_TITLE
fi
export PS1='${UPDATE_XTERM_TITLE}[last: ${timer_show}s] \[\033[${PROMPT_COLOR}m\]\u@\H \w\n: \[\033[0m\]'

[[ -r ~/.bashrc_home ]] && . ~/.bashrc_home
