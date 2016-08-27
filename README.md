# dotfiles
Environment and editor config files

## Installation
"Installation" means copying the config files into the proper locations.
`install.py` is a script that does this automatically:
```
./install.py
```

To see the script's options:
```
./install.py -h
```

## Reminder

Notes from various *manpages*.

### Bash:
```
       /bin/bash
              The bash executable
       /etc/profile
              The systemwide initialization file, executed for login shells
       ~/.bash_profile
              The personal initialization file, executed for login shells
       ~/.bashrc
              The individual per-interactive-shell startup file
       ~/.bash_logout
              The individual login shell cleanup file, executed when  a  login
              shell exits
       ~/.inputrc
              Individual readline initialization file
```

`bind -p` for readline keybindings

