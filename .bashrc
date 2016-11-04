### ENV VARS ###
# For M-x shell
export PATH="/usr/local/bin:$PATH"
export PYTHONSTARTUP="$HOME/.pythonrc.py"

# Bash-specific variables
export HISTSIZE=10
export HISTCONTROL=ignoreboth
export HISTIGNORE='[bf]g:exit:history:history *'



### BASH OPTIONS ###
set -o ignoreeof # Prevent accidental logouts when hitting C-d
set -o notify # Notify me asynchronously when background jobs finish
set -o nolog # Don't log history
shopt -s -q cdspell # Spell-check paths
if [ "${BASH_VERSINFO[0]}" -ge 4 ]; then
    shopt -s -q direxpand # Expand directories (>= v4)
    shopt -s -q dirspell # Spell-check directories (>= v4) 
    shopt -s -q globstar # Support the ** glob pattern (>= v4)
fi
shopt -s -q cmdhist # Save multi-line cmds
shopt -s -q lithist # Store multi-line cmds with newlines instead of ;
shopt -s -q checkwinsize # Update rows/columns as necessary after each cmd



### ALIASES ###
alias which='type -a'
alias rm='\rm -iv'
alias cp='\cp -iv'
alias mv='\mv -iv'
alias gdb='\gdb --quiet'
alias hist='history | less'
if [ `uname` = Linux ]; then
    alias ls='\ls -AFghv --time-style=long-iso'
else
    alias ls='\ls -AFgh'
fi
alias l='\ls -1F' # Much faster than "ls"
alias ipython='\ipython --no-banner --no-confirm-exit --classic'


### PROMPT ###
## Inspired by Ville Laurikari's SO answer: http://stackoverflow.com/a/1862762
function timer-start {
    timer=${timer:-$SECONDS}
}
function timer-stop {
    local nsecs=$(($SECONDS - $timer))
    TIMER_VALUE=${nsecs}s
    if [ $nsecs -gt 3600 ]; then
        TIMER_VALUE="$(($nsecs / 3600))h$((($nsecs % 3600) / 60))m$((($nsecs % 3600) % 60))s"
    elif [ $nsecs -gt 60 ]; then
        TIMER_VALUE="$(($nsecs / 60))m$(($nsecs % 60))s"
    fi
    unset timer
}
set -o functrace # Enable traps to be inherited by shell functions
trap 'timer-start' DEBUG
export PROMPT_COMMAND=timer-stop

COLOR_NORMAL="\[\033[0m\]"
COLOR_LIGHTRED="\[\033[31m\]"
COLOR_GREEN="\[\033[32m\]"
COLOR_YELLOW="\[\033[33m\]"
STATUS_OK=${COLOR_YELLOW}ok${COLOR_NORMAL}
STATUS_NOK=${COLOR_LIGHTRED}err${COLOR_NORMAL}
STATUS="\`if [ \$? = 0 ]; then echo ${STATUS_OK}; else echo ${STATUS_NOK}; fi\`"
export PS1="[${STATUS} \${TIMER_VALUE}] ${COLOR_GREEN}\u@\H \w\n: ${COLOR_NORMAL}"



### FUNCTIONS ###
function checkout-pr {
    if [ $# -ne 2 ]; then
        echo "checkout-pr PR_ID BRANCH_NAME"
        return 1
    fi
    local pr_id="$1"
    local branch_name="$2"
    git fetch origin "pull/${pr_id}/head:${branch_name}" && \
        git checkout "$branch_name"
}



### MISC ###
[ -n "`which xmodmap 2>/dev/null`" -a -r ~/.Xmodmap ] && xmodmap ~/.Xmodmap
>>>>>>> 8359aacb0c36b090e5493d510a96c3707c2ecb19

[ -r ~/.bashrc_home ] && . ~/.bashrc_home
[ -r ~/.bashrc_work ] && . ~/.bashrc_work
