HOME_CONFS := bash emacs postgres python readline sqlite x11


.DEFAULT : help
.PHONY : linux mac $(ALL_CONFS)

help :
	@echo -e "Usage:\n" \
	"    make linux\n" \
	"    make mac\n" \

$(HOME_CONFS) :
	stow -v -t ~ $@

Library : /Library
	stow -v -t /Library Library

etc : /etc
	stow -v -t /etc etc

linux : etc $(HOME_CONFS)
mac : Library $(HOME_CONFS)
