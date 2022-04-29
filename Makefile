# Why use stow when you could use makefile to do the same thing?
# Because I can see anything, which I mean control anything with Makefile directly

# for CI purpose
CONFIG_DIR_NAME ?=.config

help:
	@echo "make install       -  move $(HOME)/$(CONFIG_DIR_NAME) $(HOME)/$(CONFIG_DIR_NAME)_backup"
	@echo "make dry-uninstall -  a dry run of uninstall"
	@echo "make uninstall     -  require make dry-uninstall"
	@echo "                      rm $(HOME)/$(CONFIG_DIR_NAME)"
	@echo "                      move $(HOME)/$(CONFIG_DIR_NAME)_back $(HOME)/$(CONFIG_DIR_NAME)"
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
	@echo "move ~/$(CONFIG_DIR_NAME) to ~/$(CONFIG_DIR_NAME)_backup"
	mv $(HOME)/$(CONFIG_DIR_NAME) $(HOME)/$(CONFIG_DIR_NAME)_backup
	ln -s $(PWD) $(HOME)/$(CONFIG_DIR_NAME) && echo "Install done!"
.PHONY: install

dry-uninstall:
	@echo "==> check $(HOME)/$(CONFIG_DIR_NAME) as symlink"
	@[ -L "$(HOME)/$(CONFIG_DIR_NAME)" ]
	@echo "$(HOME)/$(CONFIG_DIR_NAME) linked"
	@echo
	@echo "==> check $(HOME)/$(CONFIG_DIR_NAME) existence"
	@[ -e "$(HOME)/$(CONFIG_DIR_NAME)" ]
	@echo "$(HOME)/$(CONFIG_DIR_NAME) installed"
	@echo
	@echo "==> check $(HOME)/$(CONFIG_DIR_NAME)_backup existence"
	@[ -e "$(HOME)/$(CONFIG_DIR_NAME)_backup" ]
	@echo "$(HOME)/$(CONFIG_DIR_NAME)_backup exists"
	@exit 0
.PHONY: dry-uninstall

uninstall: dry-uninstall
	@echo "delete $(HOME)/$(CONFIG_DIR_NAME) and move $(HOME)/$(CONFIG_DIR_NAME)_backup to $(HOME)/$(CONFIG_DIR_NAME)"
	rm $(HOME)/$(CONFIG_DIR_NAME)
	mv $(HOME)/$(CONFIG_DIR_NAME)_backup $(HOME)/$(CONFIG_DIR_NAME)
	echo "Uninstall done!"

# vim:ft=make
