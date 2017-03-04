HOME_CONFS := bash emacs postgres python readline sqlite x11


.DEFAULT : help
.PHONY : linux mac $(HOME_CONFS)

help :
	@echo "Usage:"
	@echo "    make linux"
	@echo "    make mac"

$(HOME_CONFS) :
	stow -v -t ~ $@

Library : /Library
	sudo stow -v -t /Library Library

etc : /etc
	sudo stow -v -t /etc etc

linux : etc $(HOME_CONFS) ;
mac : Library $(HOME_CONFS) ;
