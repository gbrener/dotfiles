#### ENV VARS ####
# customize command prompt display
export PS1='\u@\H \w: '

# set default editor to emacs
# this comes in handy when in "less" (press 'v' to enter emacs)
export ALTERNATE_EDITOR="emacs"
export EDITOR="emacsclient -s $HOME/.emacs.d/sock -c -a ''"
export VISUAL=$EDITOR

# set default options for "less" command (display "long prompt" and operate quietly)
export LESS="-MQR"

# don't log history for duplicate commands or commands that begin with spaces
export HISTCONTROL=ignoreboth

# add Anaconda to path
export PATH="$HOME/anaconda3/bin:$PATH"


#### OPTIONS ####
# spell-check and expand directories/paths (cdspell, dirspell, direxpand)
shopt -s cdspell 2>/dev/null
shopt -s direxpand 2>/dev/null
shopt -s dirspell 2>/dev/null

# store multi-line cmds with embedded newlines in history (cmdhist, lithist)
shopt -s cmdhist 2>/dev/null
shopt -s lithist 2>/dev/null

# update rows, columns as necessary after each cmd is executed (checkwinsize)
shopt -s checkwinsize 2>/dev/null

# support the ** glob pattern (globstar)
shopt -s globstar 2>/dev/null

# map ctrl to caps_lock
test `uname` = Darwin || setxkbmap -option ctrl:nocaps

test `uname` = Darwin && export HISTSIZE=30

#### ALIASES ####
alias vi='vim'
alias rm='rm -i'
alias cp='cp -iv'
alias mv='mv -iv'
alias gdb='gdb --quiet'
alias grep='grep --color=always'
alias hist='history | less'
alias l='\ls -1F'
if [ ! `uname` = Darwin ]
then
    alias ls='\ls -vAlhF --time-style=long-iso'
else
    alias ls='\ls -AlhF'
fi


#### FUNCTIONS ####
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
