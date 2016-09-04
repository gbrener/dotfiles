# Load .profile if it's readable
[[ -r ~/.profile ]] && . ~/.profile

# Load .bashrc if it's readable
[[ -r ~/.bashrc ]] && . ~/.bashrc

# Initialize X session if there's a display
[[ -z $DISPLAY && $XDG_VTNR -eq 1 ]] && exec startx
