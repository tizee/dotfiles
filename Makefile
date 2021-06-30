# Why use stow when you could use makefile to do the same thing?
all: install

install:
	@echo "move ~/.config to ~/.config_backup"
	-mv ~/.config ~/.config_backup
	-ln -s $(PWD) ~/.config && print "Install done!"

uninstall: 
	@echo "delete ~/.config and move ~/.config_backup to ~/.config"
	-rm ~/.config && mv ~/.config_backup ~/.config  && print "Uninstall done!"

.PHONY: install uninstall

# vim:ft=make
