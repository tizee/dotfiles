<div align="center">

# Dotfiles

![Last commit](https://img.shields.io/github/last-commit/tizee/dotfiles?style=flat-square)

My dotfiles for Unix-like systems (MacOS and Arch Linux).

</div>

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
