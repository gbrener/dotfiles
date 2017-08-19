### ENV VARS ###
# For M-x shell
export PATH="/usr/local/bin:$PATH"
export PYTHONSTARTUP="$HOME/.pythonrc.py"

# Bash-specific variables
export HISTCONTROL=erasedups:ignoreboth
export HISTIGNORE='[bf]g:exit:history:history *'
export HISTTIMEFORMAT="%F %T "


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



### PROMPT ###
## Inspired by Ville Laurikari's SO answer: http://stackoverflow.com/a/1862762
function timer-start {
    if [ -z "$last_cmd" ]; then
        last_cmd="$BASH_COMMAND"
    fi
    timer=${timer:-$SECONDS}
}
function timer-stop {
    local nsecs=$(($SECONDS - $timer))
    TIMER_VALUE="${nsecs}s"
    if [ $nsecs -gt 3600 ]; then
        TIMER_VALUE="$(($nsecs / 3600))h$((($nsecs % 3600) / 60))m$((($nsecs % 3600) % 60))s"
        echo "\"$last_cmd\" took $TIMER_VALUE"
    elif [ $nsecs -gt 60 ]; then
        TIMER_VALUE="$(($nsecs / 60))m$(($nsecs % 60))s"
        echo "\"$last_cmd\" took $TIMER_VALUE"
    fi
    unset timer
    unset last_cmd
}
if [ $OSTYPE != "darwin" ]; then
    trap - DEBUG
    trap 'timer-start' DEBUG
    export PROMPT_COMMAND=timer-stop
fi

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

function has-opt {
    if [ $# -ne 2 ]; then
        echo -e "has-opt: Check if a program supports an option.\n"\
            "usage: has-opt PROG OPT\n"\
            "example:"\
            "\t$ has-opt ls --time-style\n"\
            "\tyes"
        return 1
    fi
    local prog="$1"
    local opt="$2"
    if strings `\which "${prog}"` | grep -q -c -m1 "^\s*${opt}\b"; then
        echo "yes"
    else
        echo "no"
        return 1
    fi
}

function perms-mode {
    # In retrospect, writing this function in bash was probably a mistake. But it works!
    if [ $# -ne 1 -o ${#1} -ne 9 ]; then
        echo -e "perms-mode: Display integer mode that represents a given 9-character permissions string. Does not handle the \"special s/t/X modes\" (setuid/gid, sticky, nor special-execute).\n"\
            "usage: perms-mode PERMS\n"\
            "example:"\
            "\t$ perms-mode rw-r--r--\n"\
            "\t644"
        return 1
    fi
    local perms="$1"
    local mode=0
    local factor=1
    local bit=-
    # Iterate over the permission bits in reverse
    local ct=0
    for idx in `seq $((${#perms} - 1)) -1 0`; do
        if [ $ct -gt 0 -a \
             $(( $ct % 3 )) -eq 0 ]; then
            factor=$(( factor * 10 ))
        fi
        bit="${perms:${idx}:1}"
        if [ "$bit" = "r" ]; then
            mode=$(( $mode + (4 * $factor) ))
        elif [ "$bit" = "w" ]; then
            mode=$(( $mode + (2 * $factor) ))
        elif [ "$bit" = "x" ]; then
            mode=$(( $mode + (1 * $factor) ))
        fi
        ct=$((ct + 1))
    done
    echo $mode
}

function update-fork {
    if [ $# -ne 1 ]; then
        echo -e "update-fork: Update a GitHub fork with updates from its upstream sibling.\n"\
            "usage: update-fork <git upstream url>\n"\
            "example: update-fork https://github.com/conda-forge/staged-recipes.git\n"
        return 1
    fi
    local upstream_url="$1"
    git remote -v
    echo "Adding upstream remote..."
    remote add upstream "$upstream_url"
    git remote -v
    git fetch upstream
    git merge upstream/master
    echo "To finish update, run:\n\tgit push origin master"
}



### ALIASES ###
alias which='type -a'
alias rm='\rm -iv'
alias cp='\cp -iv'
alias mv='\mv -iv'
alias gdb='\gdb --quiet'
alias hist='history | less'
if has-opt ls --time-style &>/dev/null; then
    alias ls='\ls -AFghv --time-style=long-iso'
else
    alias ls='\ls -AFgh'
fi
alias l='\ls -1F' # Much faster than "ls"
alias emacs="$EDITOR"
#alias python='ipython --no-banner --no-confirm-exit --simple-prompt --classic --no-automagic "$@"'
#alias jupyter="jupyter notebook --NotebookApp.iopub_data_rate_limit=0"
alias grep='grep --color=always -I'



### MISC ###
[ -n "$INSIDE_EMACS" -a -z "$DISPLAY" ] && export DISPLAY=:0
[ -n "$INSIDE_EMACS" -a -z "$XAUTHORITY" ] && export XAUTHORITY="$HOME/.Xauthority"
#[ -n "`which xmodmap 2>/dev/null`" -a -r ~/.Xmodmap ] && xmodmap ~/.Xmodmap

[ -r ~/.bashrc_home ] && . ~/.bashrc_home
[ -r ~/.bashrc_work ] && . ~/.bashrc_work
