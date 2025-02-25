### ENV VARS ###
export LANG="en_US.UTF-8"
export PS1="\u@\h: "
export HISTCONTROL=erasedups:ignoreboth
export HISTIGNORE='[bf]g:exit:history:history *'
export HISTTIMEFORMAT="%F %T "

# For M-x shell
export PATH="/usr/local/bin:$PATH"
export PYTHONSTARTUP="$HOME/.pythonrc.py"


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



### FUNCTIONS ###
function zstd_pkg()
{
    zstd -c -T0 --ultra -20 -
}


### ALIASES ###
alias which='type -a'
alias rm='\rm -iv'
alias cp='\cp -iv'
alias mv='\mv -iv'
alias gdb='\gdb --quiet'
alias hist='history | less'
alias ls='\ls -AFgh'
alias l='\ls -1F' # Much faster than "ls"
alias emacs="$EDITOR"
#alias python='ipython --no-banner --no-confirm-exit --simple-prompt --classic --no-automagic "$@"'
#alias jupyter="jupyter notebook --NotebookApp.iopub_data_rate_limit=0"


### MISC ###
[ -n "$INSIDE_EMACS" -a -z "$DISPLAY" ] && export DISPLAY=:0
[ -n "$INSIDE_EMACS" -a -z "$XAUTHORITY" ] && export XAUTHORITY="$HOME/.Xauthority"
#[ -n "`which xmodmap 2>/dev/null`" -a -r ~/.Xmodmap ] && xmodmap ~/.Xmodmap

[ -r ~/.bashrc_home ] && . ~/.bashrc_home
[ -r ~/.bashrc_work ] && . ~/.bashrc_work
