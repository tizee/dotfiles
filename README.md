<div align="center">

# Dotfiles

My dotfiles for Unix-like systems (MacOS and Arch Linux).

</div>

![ci](https://github.com/tizee/dotfiles/actions/workflows/ci.yaml/badge.svg)
![Last commit](https://img.shields.io/github/last-commit/tizee/dotfiles?style=flat-square)

![](https://user-images.githubusercontent.com/33030965/150770997-7abd15ec-0882-41ea-911e-83241cc4b306.png)

## Installation

```
run make install first

make install       -  mv $HOME/.config $HOME/.config_backup
make dry-uninstall -  a dry run of uninstall
make uninstall     -  require make dry-uninstall
                      1. rm $HOME/.config
                      2. mv $HOME/.config_back $HOME/.config
make emacs         -  require make install
make emacs-force   -  require make install
make nvim          -  require make install
make nvim-force    -  require make install
make zsh           -  require make install
make zsh-force     -  require make install
make rm-zsh        -  require make install
```

> I prefer `make` over `stow` so long as I could control the installation granuality.

## Packages

Here are some packages why I choose to use.

### Terminal Emulator

I'd recommend [Wezterm](https://github.com/wez/wezterm) for:

1. Font ligature
2. Key assignment
3. Use lua for configuration
4. Commit frequently

BTW, leader key bug has been fixed in [#1409](https://github.com/wez/wezterm/issues/1409). Now I could use it for my daily work with `use_ime=true` to type in non-latin words.

### Editor

I use Neovim and Vim. I try my best to maintain the compatibility of configuration between NeoVim and Vim. For example, some vim plugins use functions that Neovim hadn't implemented yet or Neovim uses different names. In such cases, I have to write some plugins myself.

I do not use Emacs often but I'd like to maintain a minimal configuration of Emacs.

### Shell

Zsh is more powerful than Bash in my opinion for its zle and completion system that enables autoloaded functions.

[Zsh Completion System](https://zsh.sourceforge.io/Doc/Release/Completion-System.html)

- Use `gitstatus` in order to enable faster git prompt in zsh
  - [gitstatus](https://github.com/romkatv/gitstatus)
- zsh completion idea from Phantas0s's article, see his dotfiles [Phantas0s/.dotfiles](https://github.com/Phantas0s/.dotfiles)

## Acknowledgement

I have learned a lot from others dotfiles or scripts from which I could learn to write my own version of scripts to suit my need.
