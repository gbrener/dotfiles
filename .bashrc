#################
#### ENVIRON ####
#################
# customize command prompt display
PROMPT_COLOR=32
export PS1='\[\033[${PROMPT_COLOR}m\]\u@\H \w: \[\033[0m\]'

# set default editor to emacs
# this comes in handy when in "less" (press 'v' to enter emacs)
export ALTERNATE_EDITOR=emacs
export EDITOR="emacsclient -s $HOME/.emacs.d/sock -c -a ''"
export VISUAL=$EDITOR

# set default options for "less" command (display "long prompt" and operate quietly)
export LESS="-MQR"

# don't log history for duplicate commands or commands that begin with spaces
export HISTCONTROL=ignoreboth

# add Anaconda to path
export PATH="$HOME/anaconda3/bin:$PATH"


#################
#### OPTIONS ####
#################
# prevent accidental logouts when hitting C-d
set -o ignoreeof

# notify me asynchronously when background jobs finish
set -o notify

# don't repeat commands as they are executed
set +o verbose
set +o xtrace

# don't log history
#set -o nolog

# spell-check and expand directories/paths (cdspell, dirspell, direxpand)
shopt -s -q cdspell
shopt -s -q direxpand
shopt -s -q dirspell

# store multi-line cmds with embedded newlines in history (cmdhist, lithist)
shopt -s -q cmdhist
shopt -s -q lithist

# update rows, columns as necessary after each cmd is executed (checkwinsize)
shopt -s -q checkwinsize

# support the ** glob pattern (globstar)
shopt -s -q globstar
v
# Map Ctrl to Caps Lock (if necessary)
test `uname` = Darwin || setxkbmap -option ctrl:nocaps

# limit history to 30 commands when on Mac
test `uname` = Darwin && export HISTSIZE=30


#################
#### ALIASES ####
#################
alias vi='\vim'
alias rm='\rm -i'
alias cp='\cp -iv'
alias mv='\mv -iv'
alias gdb='\gdb --quiet'
alias grep='\grep --color=always'
alias hist='history | less'
alias l='\ls -1F'
if [ ! `uname` = Darwin ]
then
    alias ls='\ls -vAlhF --time-style=long-iso'
else
    alias ls='\ls -AlhF'
fi


#################
####  UTILS  ####
#################
# extract: extract any known compressed file type
#          got from internet
function extract () {
    if [ -f $1 ]; then
        case $1 in
            *.tar.bz2) tar xvjf $1 ;;
            *.tar.gz)  tar xvzf $1 ;;
            *.bz2)     bunzip2 $1 ;;
            *.rar)     unrar x $1 ;;
            *.gz)      gunzip $1 ;;
            *.tar)     tar xvf $1 ;;
            *.tbz2)    tar xvjf $1 ;;
            *.tgz)     tar xvzf $1 ;;
            *.zip)     unzip $1 ;;
            *.Z)       uncompress $1 ;;
            *.z)       uncompress $1 ;;
            *.7z)      7z x $1 ;;
            *)         echo "don't know how to extract '$1'..." ;;
        esac
    else
        echo "'$1' is not a valid file."
    fi
}

if [ -f ~/.bashrc_blog ]
then
    source ~/.bashrc_blog
fi
