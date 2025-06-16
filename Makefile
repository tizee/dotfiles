# Why use stow when you could use makefile to do the same thing?
# Because I can see anything, which I mean control anything with Makefile directly

# for CI purpose
CONFIG_DIR_NAME ?=.config
USER ?=$$(whoami)

help:
	@echo "run make install first"
	@echo
	@echo "make install       -  mv $(HOME)/$(CONFIG_DIR_NAME) $(HOME)/$(CONFIG_DIR_NAME)_backup"
	@echo "make dry-uninstall -  a dry run of uninstall"
	@echo "make uninstall     -  require make dry-uninstall"
	@echo "                      rm $(HOME)/$(CONFIG_DIR_NAME)"
	@echo "                      move $(HOME)/$(CONFIG_DIR_NAME)_back $(HOME)/$(CONFIG_DIR_NAME)"
	@echo "make hammerspoon   - (mac) require make install"
	@echo "make cargo         -  require make install"
	@echo "make git           -  require make install"
	@echo "make emacs         -  require make install"
	@echo "make emacs-force   -  require make install"
	@echo "make tmux          -  require make install"
	@echo "make nvim          -  require make install"
	@echo "make nvim-force    -  require make install"
	@echo "make zsh           -  require make install"
	@echo "make zsh-force     -  require make install"
	@echo "make rm-zsh        -  require make install"
	@echo "make sudoer        -  (macOS) sudoer setup"
	@echo "make lldb          -  (macOS) lldb setup"
.PHONY: help

cargo:
	@echo "link cargo config"
	mkdir -p ~/.cargo
	ln -sv $(PWD)/mac_configs/cargo/* ~/.cargo
.PHONY: cargo

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

hammerspoon:
	@echo "link hammerspoon config"
	ln -sv $(PWD)/hammerspoon $(HOME)/.hammerspoon
.PHONY: hammerspoon

emacs:
	@echo "link emacs config"
	ln -sv $(PWD)/emacs ~/.emacs.d
.PHONY: emacs

tmux:
	@echo "install tpm for tmux"
	git clone --depth=1 https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm
.PHONY: tmux

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
	curl -fLo $(HOME)/.vim/autoload/plug.vim --create-dirs https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
	# install packages with vim-plug
	# reference from https://github.com/junegunn/vim-plug/issues/225
	nvim -Es +PlugInstall +visual +qall
.PHONY: nvim

nvim-force:
	@echo "force to link nvim config"
	ln -svf $(PWD)/nvim/vimrc.vim ~/.vimrc
	# idea vim
	ln -svf $(PWD)/nvim/vimrc.vim ~/.ideavimrc
.PHONY: nvim-force

# https://apple.stackexchange.com/questions/398656/sudoers-file-resets-with-every-macos-update/398669#398669
# This enable me to skip password when sudo is required.
sudoer:
	@echo "add $$(whoami) to /private/etc/sudoers.d"
	-echo "$$(whoami) ALL=(ALL) NOPASSWD:ALL" | sudo tee /private/etc/sudoers.d/$$(whoami)
.PHONY: sudoer

lldb:
	@echo "link ~/.lldbinit to ~/.config/lldb/.lldbinit"
	ln -svf $(PWD)/lldb/.lldbinit ~/.lldbinit
.PHONY: lldb

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
