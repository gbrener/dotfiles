# For M-x shell
export PATH="/usr/local/bin:$PATH"
export PYTHONSTARTUP="$HOME/.pythonrc.py"

# Bash-specific environment variables
export HISTSIZE=10
export HISTCONTROL=ignoreboth
export HISTIGNORE='[bf]g:exit:history:history *'

# Call timer_start() at the beginning of commands and scripts, and
# timer_stop() when each command finishes.
function timer_start {
  timer=${timer:-$SECONDS}
}
function timer_stop {
  local nsecs=$(($SECONDS - $timer))
  TIMER_VALUE=${nsecs}s
  if [ $nsecs -gt 3600 ]; then
      TIMER_VALUE="$(($nsecs / 3600))h$((($nsecs % 3600) / 60))m$((($nsecs % 3600) % 60))s"
  elif [ $nsecs -gt 60 ]; then
      TIMER_VALUE="$(($nsecs / 60))m$(($nsecs % 60))s"
  fi
  unset timer
}
set -o functrace
trap 'timer_start' DEBUG
export PROMPT_COMMAND=timer_stop

# Update the prompt
if [ "$TERM" = xterm ]; then
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
export PS1='${UPDATE_XTERM_TITLE}['${STATUS}' ${TIMER_VALUE}] '${COLOR_GREEN}'\u@\H \w\n: '${COLOR_NORMAL}

set -o ignoreeof # Prevent accidental logouts when hitting C-d
set -o notify # Notify me asynchronously when background jobs finish
#set -o nolog # Don't log history
shopt -s -q cdspell # Spell-check paths
if [ "${BASH_VERSINFO[0]}" -ge 4 ]; then
    shopt -s -q direxpand # Expand directories (>= v4)
    shopt -s -q dirspell # Spell-check directories (>= v4) 
    shopt -s -q globstar # Support the ** glob pattern (>= v4)
fi
shopt -s -q cmdhist # Save multi-line cmds
shopt -s -q lithist # Store multi-line cmds with newlines instead of ;
shopt -s -q checkwinsize # Update rows/columns as necessary after each cmd

alias which='type -a'
alias rm='\rm -iv'
alias cp='\cp -iv'
alias mv='\mv -iv'
alias gdb='\gdb --quiet'
alias hist='history | less'
alias l='\ls -1FA'
alias ns='netstat -xaupen'
if [ `uname` = Linux ]; then
    alias ls='\ls -AFghv --time-style=long-iso'
else
    alias ls='\ls -AFgh'
fi
alias ipython='\ipython --no-banner --no-confirm-exit --classic'


[ -n "`which xmodmap 2>/dev/null`" -a -r ~/.Xmodmap ] && xmodmap ~/.Xmodmap

[ -r ~/.bashrc_home ] && . ~/.bashrc_home
[ -r ~/.bashrc_work ] && . ~/.bashrc_work
