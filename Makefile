# Why use stow when you could use makefile to do the same thing?
all: help

help:
	echo "make linkzsh   - link zsh config"
	echo "make cleanup   - remove zsh config"
	echo "make install   - move ~/.config to ~/.config_backup"
	echo "make uninstall - remove ~/.config and move ~/.config_backup to ~/.config"

linkzsh:
	@echo "link zsh config"
	-ln -svf $(PWD)/zshrc ~/.zshrc
	-ln -svf $(PWD)/zprofile ~/.zprofile
	-ln -svf $(PWD)/zlogin ~/.zlogin
	-ln -svf $(PWD)/zshenv ~/.zshenv

cleanup:
	-rm -v ${HOME}/.zlogin ${HOME}/.zprofile ${HOME}/.zshenv ${HOME}/.zshrc

install:
	@echo "move ~/.config to ~/.config_backup"
	-mv ~/.config ~/.config_backup
	-ln -s $(PWD) ~/.config && print "Install done!"

uninstall: 
	@echo "delete ~/.config and move ~/.config_backup to ~/.config"
	-rm ~/.config && mv ~/.config_backup ~/.config  && print "Uninstall done!"

.PHONY: all install uninstall

# vim:ft=make
