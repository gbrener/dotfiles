if [[ `uname` = Linux ]]; then
    if `which setxkbmap`; then
        setxkbmap -option ctrl:nocaps
    else
        echo "Warning: setxkbmap not available. Caps Lock remapping unsucessful."
    fi
fi

PROMPT_COLOR=32 # green
export PS1='\[\033[${PROMPT_COLOR}m\]\u@\H \w\n: \[\033[0m\]'
export VISUAL="emacsclient -s $HOME/.emacs.d/sock -c -a ''"
export LESS="-MQR" # Enable long-prompt, quiet-operation, and ansi colors in "less"
export HISTCONTROL=ignoreboth
test `uname` = Darwin && export HISTSIZE=30
export MOZ_USE_OMTC=1 # Enable hardware acceleration in Firefox
export PYTHONSTARTUP="$HOME/.pythonrc.py"
export PATH="$HOME/anaconda3/bin:$PATH"

set -o ignoreeof # Prevent accidental logouts when hitting C-d
set -o notify # Notify me asynchronously when background jobs finish
#set -o nolog # Don't log history
shopt -s -q cdspell # Spell-check paths
shopt -s -q direxpand # Expand directories
shopt -s -q dirspell # Spell-check directories
shopt -s -q cmdhist # Save multi-line cmds
shopt -s -q lithist # Store multi-line cmds with newlines instead of ;
shopt -s -q checkwinsize # Update rows/columns as necessary after each cmd
shopt -s -q globstar # Support the ** glob pattern

alias emacs=$VISUAL
alias rm='\rm -iv'
alias cp='\cp -iv'
alias mv='\mv -iv'
alias gdb='\gdb --quiet'
alias hist='history | less'
alias l='\ls -1F'
test `uname` = Darwin && alias ls='\ls -AFgh'
test `uname` = Linux && alias ls='\ls -AFghv --time-style=long-iso'
alias install_pkg='makepkg -sri'

if [ -f ~/.bashrc_home ]; then
    source ~/.bashrc_home
fi
