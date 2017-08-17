# Load .profile if it's readable
[[ -r ~/.profile ]] && . ~/.profile

# Remap CTRL to CAPS-LOCK
#if [ `uname` = Linux ]; then
#    if [ -x /usr/bin/setxkbmap ]; then
#        /usr/bin/setxkbmap -option ctrl:nocaps
#    else
#        echo "Warning: setxkbmap not available. Caps Lock remapping unsuccessful."
#    fi
#fi

# Load .bashrc if it's readable
[[ -r ~/.bashrc ]] && . ~/.bashrc

# Initialize X session if there's a display
[[ -z $DISPLAY && $XDG_VTNR -eq 1 ]] && exec startx
if [ -e /home/greg/.nix-profile/etc/profile.d/nix.sh ]; then . /home/greg/.nix-profile/etc/profile.d/nix.sh; fi # added by Nix installer
