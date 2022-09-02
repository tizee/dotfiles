# Why use stow when you could use makefile to do the same thing?
# Because I can see anything, which I mean control anything with Makefile directly

# for CI purpose
CONFIG_DIR_NAME ?=.config

help:
	@echo "run make install first"
	@echo
	@echo "make install       -  mv $(HOME)/$(CONFIG_DIR_NAME) $(HOME)/$(CONFIG_DIR_NAME)_backup"
	@echo "make dry-uninstall -  a dry run of uninstall"
	@echo "make uninstall     -  require make dry-uninstall"
	@echo "                      rm $(HOME)/$(CONFIG_DIR_NAME)"
	@echo "                      move $(HOME)/$(CONFIG_DIR_NAME)_back $(HOME)/$(CONFIG_DIR_NAME)"
	@echo "make git           -  require make install"
	@echo "make emacs         -  require make install"
	@echo "make emacs-force   -  require make install"
	@echo "make nvim          -  require make install"
	@echo "make nvim-force    -  require make install"
	@echo "make zsh           -  require make install"
	@echo "make zsh-force     -  require make install"
	@echo "make rm-zsh        -  require make install"
.PHONY: help

git:
	@echo "link git config"
	ln -sv $(PWD)/git/common.gitconfig ~/.gitconfig
.PHONY: git

# use ln -svf is dangerous
zsh:
	@echo "link zsh config"
	ln -sv $(PWD)/zshrc ~/.zshrc
	ln -sv $(PWD)/zprofile ~/.zprofile
	ln -sv $(PWD)/zlogin ~/.zlogin
	ln -sv $(PWD)/zshenv ~/.zshenv
.PHONY: zsh

zsh-force:
	@echo "link zsh config"
	ln -svf $(PWD)/zshrc ~/.zshrc
	ln -svf $(PWD)/zprofile ~/.zprofile
	ln -svf $(PWD)/zlogin ~/.zlogin
	ln -svf $(PWD)/zshenv ~/.zshenv
.PHONY: zsh-force

emacs:
	@echo "link emacs config"
	ln -sv $(PWD)/emacs ~/.emacs.d
.PHONY: emacs

emacs-force:
	@echo "force to link emacs config"
	ln -svf $(PWD)/emacs ~/.emacs.d
.PHONY: emacs-force

nvim:
	@echo "link nvim config"
	ln -svf $(PWD)/nvim/vimrc.vim ~/.vimrc
	# idea vim
	ln -svf $(PWD)/nvim/vimrc.vim ~/.ideavimrc
	# vim-plug
	curl -fLo ~/.vim/autoload/plug.vim --create-dirs https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
.PHONY: nvim

nvim-force:
	@echo "force to link nvim config"
	ln -svf $(PWD)/nvim/vimrc.vim ~/.vimrc
	# idea vim
	ln -svf $(PWD)/nvim/vimrc.vim ~/.ideavimrc
.PHONY: nvim-force

rm-zsh:
	rm -v $(HOME)/.zlogin $(HOME)/.zprofile $(HOME)/.zshenv $(HOME)/.zshrc
.PHONY: nvim-force

install:
	@echo "move ~/$(CONFIG_DIR_NAME) to ~/$(CONFIG_DIR_NAME)_backup"
	-mv $(HOME)/$(CONFIG_DIR_NAME) $(HOME)/$(CONFIG_DIR_NAME)_backup > /dev/null
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
