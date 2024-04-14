# nvim

I'd like to keep the configuration simple, but, mind changed since NeoVim used.

## Roadmap

- [ x ] Nvim Lua configuration which makes code reading easier
- [ x ] Isolate Nvim only configuration

## Vim Compatibility

`~/.vimrc` is a symbolic link to `vimrc.vim`.

## Vim default plugins

### homebrew

- vim9.1

```
> ls $(brew --prefix vim)/share/vim/vim91
autoload
bugreport.vim
colors
compiler
defaults.vim
delmenu.vim
doc
evim.vim
filetype.vim
ftoff.vim
ftplugin
ftplugin.vim
ftplugof.vim
gvimrc_example.vim
import
indent
indent.vim
indoff.vim
keymap
lang
macros
menu.vim
mswin.vim
optwin.vim
pack
plugin
print
scripts.vim
spell
synmenu.vim
syntax
tools
tutor
vimrc_example.vim

```

- neovim

```
> ls $(brew --prefix nvim)/share/runtime
autoload
colors
compiler
delmenu.vim
doc
filetype.lua
ftoff.vim
ftplugin
ftplugin.vim
ftplugof.vim
indent
indent.vim
indoff.vim
keymap
lua
macmap.vim
macros
makemenu.vim
menu.vim
mswin.vim
neovim.ico
optwin.vim
pack
plugin
queries
spell
synmenu.vim
syntax
tools
tutor
```
