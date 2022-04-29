# Why use stow when you could use makefile to do the same thing?
# Because I can see anything, which I mean control anything with Makefile directly

help:
	@echo "make install       -  move $(HOME)/.config $(HOME)/.config_backup"
	@echo "make dry-uninstall -  a dry run of uninstall"
	@echo "make uninstall     -  require make dry-uninstall"
	@echo "                      rm $(HOME)/.config"
	@echo "                      move $(HOME)/.config_back $(HOME)/.config"
	@echo "make emacs         -  require make install"
	@echo "make emacsforce    -  require make install"
	@echo "make nvim          -  require make install"
	@echo "make nvimforce     -  require make install"
	@echo "make zsh           -  require make install"
	@echo "make zshforce      -  require make install"
.PHONY: help

# use ln -svf is dangerous
zsh: install
	@echo "link zsh config"
	ln -sv $(PWD)/zshrc ~/.zshrc
	ln -sv $(PWD)/zprofile ~/.zprofile
	ln -sv $(PWD)/zlogin ~/.zlogin
	ln -sv $(PWD)/zshenv ~/.zshenv

zsh-force: install
	@echo "link zsh config"
	ln -svf $(PWD)/zshrc ~/.zshrc
	ln -svf $(PWD)/zprofile ~/.zprofile
	ln -svf $(PWD)/zlogin ~/.zlogin
	ln -svf $(PWD)/zshenv ~/.zshenv

emacs: install
	@echo "link emacs config"
	ln -sv $(PWD)/emacs ~/.emacs.d

emacs-force: install
	@echo "force to link emacs config"
	ln -svf $(PWD)/emacs ~/.emacs.d

nvim: install
	@echo "link nvim config"
	ln -svf $(PWD)/nvim/vimrc.vim ~/.vimrc
	# idea vim
	ln -svf $(PWD)/nvim/vimrc.vim ~/.ideavimrc

nvim-force: install
	@echo "force to link nvim config"
	ln -svf $(PWD)/nvim/vimrc.vim ~/.vimrc
	# idea vim
	ln -svf $(PWD)/nvim/vimrc.vim ~/.ideavimrc

# cleanzsh:
# 	-rm -v $(HOME)/.zlogin $(HOME)/.zprofile $(HOME)/.zshenv $(HOME)/.zshrc

install:
	@echo "move ~/.config to ~/.config_backup"
	mv $(HOME)/.config $(HOME)/.config_backup
	ln -s $(PWD) $(HOME)/.config && print "Install done!"
.PHONY: install

dry-uninstall:
	@echo "==> check $(HOME)/.config as symlink"
	@[ -L "$(HOME)/.config" ]
	@echo "$(HOME)/.config linked"
	@echo
	@echo "==> check $(HOME)/.config existence"
	@[ -e "$(HOME)/.config" ]
	@echo "$(HOME)/.config installed"
	@echo
	@echo "==> check $(HOME)/.config_backup existence"
	@[ -e "$(HOME)/.config_backup" ]
	@echo "$(HOME)/.config_backup exists"
	@exit 0
.PHONY: dry-uninstall

uninstall: dry-uninstall
	@echo "delete $(HOME)/.config and move $(HOME)/.config_backup to $(HOME)/.config"
	rm $(HOME)/.config
	print "Uninstall done!"
	# -[ -e $(HOME)/.config_backup] && mv $(HOME)/.config_backup $(HOME)/.config

# vim:ft=make
