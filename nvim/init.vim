" vim: set foldmethod=marker foldlevel=0 nomodeline:
" see :help auto-setting

let g:dracula_bold = 1
let g:dracula_italic = 1
let g:dracula_underline = 1

" workaround for italics
set t_ZH=[3m
set t_ZR=[23m

" GENERAL SETTINGS {{{

" ========== vim basic options ========= {{{
" Encoding
scriptencoding utf-8
set fileencodings=utf-8,cp936
set fileencoding=utf-8
set encoding=utf8

" true color
"For Neovim > 0.1.5 and Vim > patch 7.4.1799 < https://github.com/vim/vim/commit/61be73bb0f965a895bfb064ea3e55476ac175162 >
"Based on Vim patch 7.4.1770 (`guicolors` option) < https://github.com/vim/vim/commit/8a633e3427b47286869aa4b96f2bfc1fe65b25cd >
" < https://github.com/neovim/neovim/wiki/Following-HEAD#20160511 >
if has("termguicolors")
  set termguicolors
endif
" double with chracter
" set ambiwidth=double
" display endofline tab space enter
set nocompatible " be iMproved, required
set list
set listchars=eol:⏎,tab:»-,trail:-,nbsp:.
" show break
set showbreak=⌞
" linebreak
set linebreak

"show command line
set showcmd
set cmdheight=2

" opening message
set shortmess+=c " hit-enter

" TextEdit might fail if hidden is not set.
set hidden

" Having longer updatetime (default is 4000 ms = 4 s) leads to noticeable
" delays and poor user experience.
set updatetime=300
" By default timeoutlen is 1000 ms
set timeout 
" map sequence timeout
set timeoutlen=500
" key code timeout
set ttimeoutlen=256 

" Don't pass messages to |ins-completion-menu|.
" set shortmess+=c

" Always show the signcolumn, otherwise it would shift the text each time
" diagnostics appear/become resolved.
if has("patch-8.1.1564")
  " Recently vim can merge signcolumn and number column into one
  set signcolumn=number
else
  set signcolumn=yes
endif


" Backup and swap
" Some servers have issues with backup files, see coc.vim/#649.
set nobackup
set nowritebackup
" set noswapfile
silent !mkdir -p ~/.config/nvim/tmp/backup
silent !mkdir -p ~/.config/nvim/tmp/undo
"silent !mkdir -p ~/.config/nvim/tmp/sessions
set backupdir=~/.config/nvim/tmp/backup,.
set directory=~/.config/nvim/tmp/backup,.
" Undo settings
if has('persistent_undo')
  set undofile
  set undolevels=10240
  set undodir=~/.config/nvim/tmp/undo,.
endif

set laststatus=2
if !has('gui_running')
  set t_Co=256
  " https://github.com/dracula/vim/issues/96
  let g:dracula_colorterm = 0
endif
set noshowmode


" syntax highlighting
syntax enable
syntax on

" close sounds on erros
set noerrorbells
set novisualbell
set t_vb=
set tm=500

" ctrl-a/ctrl-x as decimal number
set nrformats=

" show linenumber
set number
set relativenumber

" check filetype
filetype on

" Tab
" shift indent
" set tabstop=8
set expandtab " Use spaces instead of tab
set shiftwidth=2
set tabstop=2
" set smartindent
set smarttab

" set autoindent off
set noautoindent

" show match
set showmatch

" show command line
set showcmd
set cmdheight=2

" Better command-line completion
set wildmenu

" close backup
" set nobackup

" show cursor location
" set ruler

" current line highlighting
" set cursorline

" highlight searchs
set hlsearch
set incsearch

" set fencs=utf-8

" menu language
set langmenu=en_US.UTF-8

" ignore cases by default
set ic

" message
" language messages en_US.utf-8

" auto-reload working directory
" autocmd BufEnter * lcd %:p:h
" }}}

" ========== neovim options ========= {{{
" see diffrences:
" :help vim_diff.txt
" ture color

if has('nvim')
  " For Neovim 0.1.3 and 0.1.4 < https://github.com/neovim/neovim/pull/2198 >
  let $NVIM_TUI_ENABLE_TRUE_COLOR=1
  set runtimepath^=~/.vim runtimepath+=~/.vim/after
  let &packpath = &runtimepath
  " Shows the effects of a command incrementally, as you type.
  set inccommand=nosplit 
  " source ~/.vimrc
  " echo $TERM
  " echo $COLORTERM => truecolor
  " curl -sSL https://raw.githubusercontent.com/robertknight/konsole/master/tests/color-spaces.pl | perl
  set termguicolors  " true color
endif

" enable python3 support for neovim
" :echo has('python3')
" 1: enable, 0: disable
let g:python3_host_prog="/usr/local/bin/python3"

"}}}

" ========== OS Specific ========== {{{

if !exists("g:os")
  if has("win64") || has("win32") || has("win16")
    let g:os = "Windows"
  else
    let g:os = substitute(system('uname'), '\n', '', '')
  endif
endif

if has("gui_running")
  if g:os == "Darwin"
    set guifont=JetBrainsMono\ Nerd\ Font\ Mono\ 16
    " set guifont=Fira\ Mono:h12
  elseif g:os == "Linux"
    set guifont=JetBrainsMono\ Nerd\ Font\ Mono\ 16
  elseif g:os == "Windows"
    set guifont=JetBrainsMono\ Nerd\ Font\ Mono\ 16
  endif
endif

" ========== Windows ========= {{{

if g:os == "Windows"
  behave mswin
  " set shell program, default is cmd
  set shell=C:\WINDOWS\System32\wsl.exe
  " set shell=\"C:\WINDOWS\System32\cmd.exe"\ -f
  set shellpipe=|
  set shellredir=>
  set shellcmdflag=/c
  " chinese menu
  source $VIMRUNTIME/delmenu.vim
  source $VIMRUNTIME/menu.vim
endif

" }}}

" fzf
if g:os == "Darwin"
  set rtp+=/usr/local/opt/fzf
endif
" }}}

" }}}

"call VIM PLUG {{{

" :PlugInstall
" :PlugUpdate
" :PlugDiff
" review changes from last update
" :PlugClean

source ~/.config/nvim/my-config/tz-packages.vim

" vim-plug cheatsheet
function! Cheatsheet_vim_plug()
  echo ':PlugInstall install plugins'
  echo ':PlugUpdate update plugins'
  echo ':PlugDiff review changes from last update'
  echo ':PlugClean autoremove plugins'
endfunction

" vim-plug key-mappings {{{
nnoremap <leader>pi :PlugInstall<cr>
nnoremap <leader>pu :PlugUpgrade<cr>
nnoremap <leader>ps :PlugStatus<cr>
nnoremap <leader>pc :PlugConfig<cr>

" }}}

" }}}

" TMUX {{{
if !empty($TMUX)

endif

" }}}

" UI {{{ 
colorscheme dracula
highlight Comment cterm=italic gui=italic
" set background=dark
" }}}

" AUTOCMD GROUP {{{
" VimScript  {{{
augroup filetype_vim
  autocmd!          | " Deletes all auto-commands in the current group
  autocmd FileType vim setlocal foldmethod=marker
  autocmd BufWritePost $MYVIMRC source $MYVIMRC
augroup END
" }}}

" ShellScript {{{
augroup filetype_shell
  autocmd!
  autocmd FileType sh setlocal foldmethod=marker
  autocmd FileType zsh setlocal foldmethod=marker
augroup END
" }}}

" tmux {{{
augroup filetype_tmux
  autocmd!
  autocmd FileType tmux setlocal foldmethod=marker
augroup END
" }}}

" yaml {{{
augroup ft_yaml
  " Remove all vimrc autocommands
  autocmd!
  autocmd FileType yaml setlocal foldmethod=indent
augroup end
" }}}

" python {{{
aug ft_python
  autocmd!          | " Deletes all auto-commands in the current group
  " ftype/python.vim overwrites this
  autocmd FileType python setlocal ts=8 sts=4 sw=8 noexpandtab
aug end
" }}}

" golang {{{
aug ft_golang
  autocmd!         | " Deletes all auto-commands in the current group
  autocmd FileType go setlocal ts=8 sts=4 sw=8 noexpandtab
  autocmd BufNewFile,BufRead *.go setlocal foldmethod=syntax
aug end
" }}}

" The following autocommand will cause the quickfix window to open after any grep invocation:
autocmd QuickFixCmdPost *grep* cwindow
" }}}

" ABBREVIATION {{{
iabbrev adn and
iabbrev waht what
iabbrev tehn then

"production}}}
