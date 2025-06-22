### ENV VARS ###
export LANG="en_US.UTF-8"
export HISTCONTROL=erasedups:ignoreboth
export HISTIGNORE='[bf]g:exit:history:history *'
export HISTTIMEFORMAT="%F %T "
export EDITOR="emacsclient -t -a ''"
export VISUAL="$EDITOR"

# https://www.cyberciti.biz/faq/bash-shell-change-the-color-of-my-shell-prompt-under-linux-or-unix/
export PS1="\[\e[0;32m\]\u@\h\[\e[1;32m\] \w\[\e[m\]: ";


# For M-x shell
export PATH="/usr/local/bin:/opt/rocm-6.3.0/bin:$PATH"
export LD_LIBRARY_PATH="/opt/rocm-6.3.0/bin:$LD_LIBRARY_PATH"


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

function encrypt_pdf()
{
    pdftk "$1" output $(IFS= read -rp 'Output PDF: ' outfile && echo $outfile) owner_pw PROMPT user_pw PROMPT;
}


### ALIASES ###
alias which='type -a'
alias rm='\rm -I -v --preserve-root'
alias cp='\cp -iv'
alias mv='\mv -iv'
alias gdb='\gdb --quiet'
alias hist='history | less'
alias ls='\ls -AFgh --color=tty'
alias l='\ls -1F' # Much faster than "ls"
alias emacs="$EDITOR"
#alias python='ipython --no-banner --no-confirm-exit --simple-prompt --classic --no-automagic "$@"'
#alias jupyter="jupyter notebook --NotebookApp.iopub_data_rate_limit=0"
alias systemctl='systemctl --full --no-pager'
alias meminfo='free -hlt'
alias ports='netstat -tulanp'
alias routes='echo "Rules:"; ip rule list | column -t -R 0; echo; echo "Neighbors:"; ip neigh | column -t; echo; echo "Routes:"; ip route show table all | column -t'

### MISC ###
[ -n "$INSIDE_EMACS" -a -z "$DISPLAY" ] && export DISPLAY=:0
[ -n "$INSIDE_EMACS" -a -z "$XAUTHORITY" ] && export XAUTHORITY="$HOME/.Xauthority"
#[ -n "`which xmodmap 2>/dev/null`" -a -r "$HOME/.Xmodmap" ] && xmodmap "$HOME/.Xmodmap"

[ -r "$HOME/.bashrc_home" ] && . "$HOME/.bashrc_home"
[ -r "$HOME/.bashrc_work" ] && . "$HOME/.bashrc_work"

# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
__conda_setup="$("$HOME/miniforge3/bin/conda" 'shell.bash' 'hook' 2> /dev/null)"
if [ $? -eq 0 ]; then
    eval "$__conda_setup"
else
    if [ -f "$HOME/miniforge3/etc/profile.d/conda.sh" ]; then
        . "$HOME/miniforge3/etc/profile.d/conda.sh"
    else
        export PATH="$HOME/miniforge3/bin:$PATH"
    fi
fi
unset __conda_setup

if [ -f "$HOME/miniforge3/etc/profile.d/mamba.sh" ]; then
    . "$HOME/miniforge3/etc/profile.d/mamba.sh"
fi
# <<< conda initialize <<<

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

# bun
export BUN_INSTALL="$HOME/.bun"
export PATH="$BUN_INSTALL/bin:$PATH"

#THIS MUST BE AT THE END OF THE FILE FOR SDKMAN TO WORK!!!
export SDKMAN_DIR="$HOME/.sdkman"
[[ -s "$HOME/.sdkman/bin/sdkman-init.sh" ]] && source "$HOME/.sdkman/bin/sdkman-init.sh"
