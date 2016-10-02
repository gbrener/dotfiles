# Bash-specific environment variables
if [ `uname` = Linux ]; then
    export HISTSIZE=1000
else
    export HISTSIZE=10
fi
export HISTCONTROL=ignoreboth
export HISTIGNORE='[bf]g:exit:history:history *'

# Call timer_start() at the beginning of commands and scripts,
# and timer_stop() when each command finishes.
function timer_start {
  timer=${timer:-$SECONDS}
}
function timer_stop {
  timer_show=$(($SECONDS - $timer))
  unset timer
}
set -o functrace
trap 'timer_start' DEBUG
export PROMPT_COMMAND=timer_stop

# Update the prompt
if [ $TERM = xterm ]; then
    UPDATE_XTERM_TITLE='\e]0;$HOSTNAME\007'
else
    unset UPDATE_XTERM_TITLE
fi
COLOR_NORMAL="\[\033[0m\]"
COLOR_LIGHTRED="\[\033[31m\]"
COLOR_GREEN="\[\033[32m\]"
COLOR_YELLOW="\[\033[33m\]"
STATUS_OK=${COLOR_YELLOW}'ok'${COLOR_NORMAL}
STATUS_NOK=${COLOR_LIGHTRED}'err'${COLOR_NORMAL}
STATUS="\`if [ \$? = 0 ]; then echo ${STATUS_OK}; else echo ${STATUS_NOK}; fi\`"
export PS1='${UPDATE_XTERM_TITLE}['${STATUS}' ${timer_show}s] '${COLOR_GREEN}'\u@\H \w\n: '${COLOR_NORMAL}

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
alias mv='\mv -iv'
alias gdb='\gdb --quiet'
alias hist='history | less'
alias l='\ls -1Fa'
alias ns='netstat -xaupen'
alias which='type -a'
if [ `uname` = Linux ]; then
    alias ls='\ls -AFghv --time-style=long-iso'
else
    alias ls='\ls -AFgh'
fi

[[ -r ~/.bashrc_home ]] && . ~/.bashrc_home
