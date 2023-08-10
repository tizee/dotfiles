<div align="center">

# Dotfiles

My dotfiles for Unix-like systems (MacOS/Arch Linux/WSL2).

</div>

![ci](https://github.com/tizee/dotfiles/actions/workflows/ci.yaml/badge.svg)
![Last commit](https://img.shields.io/github/last-commit/tizee/dotfiles?style=flat-square)

![](https://user-images.githubusercontent.com/33030965/150770997-7abd15ec-0882-41ea-911e-83241cc4b306.png)

## Installation

Clean setup directly from command line:
```
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/tizee/dotfiles/master/dotfile_scripts/install.sh)"
```

Install from github repo:
```
run make install first
make help          - show help message
make install       -  backup old .config dir and create symlink
                      1. mv $HOME/.config $HOME/.config_backup
                      2. ln -s path/to/dotfiles $HOME/.config
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

## Trade offs

- Maybe it's a good idea to use a Dockerfile to setup the development environment and reuse it from the container instead of `make install` each time on a new machine.
- But from my view, I think the frequency is too low and you have to pay a lot for this convenience later.
  - 1. your productivity tools are restricted to directories mapped to the container.

```
 docker run -v <path-to-local-dir>:/root/work -it <dotfiles-image>
```

## Packages

Here are some packages why I choose to use.

### Terminal Emulator

I'd recommend [Wezterm](https://github.com/wez/wezterm) for:

1. Font ligature
2. Key assignment
3. Use lua for configuration
4. Commit frequently

BTW, leader key bug has been fixed in [#1409](https://github.com/wez/wezterm/issues/1409). Now I could use it for my daily work with `use_ime=true` to type in non-latin words.

I sometimes use Alacritty or Kitty to test the compatibility of cli apps.

### Tmux

I build the tmux from source to use its latest features as well as wezterm nightly. This is because my workflow in tmux based on `display-popup`, which is supported for Tmux >= 3.2.

### Editor

I use Neovim and Vim. I try my best to maintain the compatibility of configuration between NeoVim and Vim. For example, some vim plugins use functions that Neovim hadn't implemented yet or Neovim uses different names. In such cases, I have to write some plugins myself.

~~I do not use Emacs often but I'd like to maintain a minimal configuration of Emacs.~~

I am trying to migrate to Emacs because the divergence between Neovim and Vim9 is tedious for me to maintain a compatible configuration.

I would continue to use Vim/NeoVim when Emacs is not convenient to use i.e. in the termianl (It's easier to configure a usable Neovim/Vim than Emacs).

### Shell

Zsh is more powerful than Bash in my opinion for its zle and completion system that enables autoloaded functions.

[Zsh Completion System](https://zsh.sourceforge.io/Doc/Release/Completion-System.html)

- Use `gitstatus` in order to enable faster git prompt in zsh
  - [gitstatus](https://github.com/romkatv/gitstatus)
- zsh completion idea from Phantas0s's article, see his dotfiles [Phantas0s/.dotfiles](https://github.com/Phantas0s/.dotfiles)

## macOS

- key
  - ~~skhd~~
  - karabiner elements
- Window related
  - ~~yabai~~
  - hammerspoon
- Editor
  - Vim 9.0 (terminal)
  - Neovim (terminal)
  - lem (CommonLisp IDE in terminal)
  - Emacs (GUI)
- Shell
  - Zsh
- Terminal Emulator
  - Kitty
  - Wezterm

## Arch Linux

- Window
  - X.Org
  - i3wm
- Program Launcher
  - Rofi
- Package manager
  - pacman
  - yay (easy to use)
- Terminal Emulator
  - Alacritty

## Acknowledgement

I have learned a lot from others dotfiles or scripts from which I could learn to write my own version of scripts to suit my need.
