#### ENV VARS ####
# customize command prompt display
export PS1='\u@\H \w: '

# set default editor to emacs
# this comes in handy when in "less" (press 'v' to enter emacs)
export ALTERNATE_EDITOR="emacs"
export EDITOR="emacsclient -s $HOME/.emacs.d/sock -c -a ''"
export VISUAL=$EDITOR

# set default options for "less" command (display "long prompt" and operate quietly)
export LESS="-MQ"

# remove duplicate commands in .history file
export HISTCONTROL=ignoredups

# add Anaconda to path
export PATH="$HOME/anaconda3/bin:$PATH"


#### OPTIONS ####
# spell-check and expand directories/paths (cdspell, dirspell, direxpand)
test -o cdspell || shopt -s cdspell
test -o direxpand || shopt -s direxpand
test -o dirspell || shopt -s dirspell

# store multi-line cmds with embedded newlines in history (cmdhist, lithist)
test -o cmdhist || shopt -s cmdhist
test -o lithist || shopt -s lithist

# update rows, columns as necessary after each cmd is executed (checkwinsize)
test -o checkwinsize || shopt -s checkwinsize

# support the ** glob pattern (globstar)
test -o globstar || shopt -s globstar

# map ctrl to caps_lock
setxkbmap -option ctrl:nocaps


#### ALIASES ####
alias vi='vim'
alias l='\ls -1F'
alias ls='\ls -vAlhF --time-style=long-iso'
alias rm='rm -i'
alias cp='cp -iv'
alias mv='mv -iv'
alias gdb='gdb --quiet'
alias grep='grep --color=always'


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

# update_blog: build a docker image serving up some static HTML/CSS/JS assets.
#              upload the image tarball to a web host and spin up a docker container.
function update_blog()
{
    if [ -z $1 ]
    then
	echo "usage: update_blog docker_image"
	return 1
    fi
    pushd $WWW_LOCAL_DIR
    make html
    sudo docker build -t $1 $WWW_LOCAL_DOCKER_DIR
    sudo docker save -o $1.tar $1
    bzip2 $1.tar
    rsync -avz --progress $1.tar.bz2 www:$WWW_HOST_DIR
    ssh www "cd $WWW_HOST_DIR; bunzip2 $1.tar.bz2; docker load -i $1.tar; docker run -d -p 80:80 $1"
    popd
}


#### INCLUDES ####
# export WWW_* env vars
source ~/.bashrc_blog
