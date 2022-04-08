" vim: set foldmethod=marker foldlevel=0 nomodeline:
" see :help auto-setting
" GENERAL SETTINGS {{{

" ========== vim basic options ========= {{{
" highlight
"set cursorcolumn
set cursorline

" workaround for italics
" https://rsapkf.xyz/blog/enabling-italics-vim-tmux
set t_ZH=[3m
set t_ZR=[23m

" Encoding
scriptencoding utf-8
set fileencodings=utf-8,cp936
set fileencoding=utf-8
set encoding=utf8
" spell check

" whether to use Python 2 or 3
set pyx=3

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

" maximum amount of memory to use for pattern matching (in Kbyte)
" avoid E363
set maxmempattern=20000
" regex magic
set magic
" modeline
set modeline

" shada
" The ShaDa file is used to store:
" - The command line history.
" - The search string history.
" - The input-line history.
" - Contents of non-empty registers.
" - Marks for several files.
" - File marks, pointing to locations in files.
" - Last search/substitute pattern (for 'n' and '&').
" - The buffer list.
" - Global variables.
if has('nvim') && ! has('win32') && ! has('win64')
  set shada=!,'100,<50,s10,h
  set shadafile=~/.config/nvim/tmp/nvim.shada
else
  set viminfo='100,<10,@50,h,n$DATA_PATH/viminfo
endif

" Secure sensitive information, disable backup files in temp directories
if exists('&backupskip')
	set backupskip+=/tmp/*,$TMPDIR/*,$TMP/*,$TEMP/*,*/shm/*,/private/var/*
	set backupskip+=.vault.vim
endif

" If sudo, disable vim swap/backup/undo/shada/viminfo writing
if $SUDO_USER !=# '' && $USER !=# $SUDO_USER
		\ && $HOME !=# expand('~'.$USER, 1)
		\ && $HOME ==# expand('~'.$SUDO_USER, 1)

	set noswapfile
	set nobackup
	set nowritebackup
	set noundofile
	if has('nvim')
		set shada="NONE"
	else
		set viminfo="NONE"
	endif
endif

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

" grepprg
set grepprg=rg\ --vimgrep
set grepformat=%f:%l:%c:%m

" Backup and swap
" Some servers have issues with backup files, see coc.vim/#649.
set nobackup
set nowritebackup
set noswapfile " not createing swap file for new buffers
" create backup
silent !mkdir -p ~/.config/nvim/tmp/backup
" create undo
silent !mkdir -p ~/.config/nvim/tmp/undo
"silent !mkdir -p ~/.config/nvim/tmp/sessions
set backupdir=~/.config/nvim/tmp/backup,.
set directory=~/.config/nvim/tmp/backup,.

" tags
set tags=./tags;,.tags,tags;,./.tags;

" Undo settings
if has('persistent_undo')
  set undofile
  set undolevels=10240
  if has('nvim')
    set undodir=~/.config/nvim/tmp/undo,.
  else
    silent !mkdir -p ~/.config/nvim/tmp/vim_undo
    set undodir=~/.config/nvim/tmp/vim_undo,.
  endif
endif

set laststatus=2
if !has('gui_running')
  set t_Co=256
  " https://github.com/dracula/vim/issues/96
  let g:dracula_colorterm = 0
else
  if has('nvim')
    autocmd UIEnter * let g:gui = filter(nvim_list_uis(),{k,v-> v.chan==v:event.chan})[0].rgb
  endif
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

" change directory automatically
set autochdir

" message
" language messages en_US.utf-8

" auto-reload working directory
" autocmd BufEnter * lcd %:p:h

" mouse support
" normal, visual mode
set mouse=nv
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
endif


"}}}

" ========== OS Specific ========== {{{

if !exists("g:os")
  if has("win64") || has("win32") || has("win16")
    let g:os = "Windows"
  else
    let g:os = substitute(system('uname -s'), '\n', '', '')
  endif
endif

if !has("gui_running")
  " Spaces after a comma are ignored.  To include a comma in a font name
	" precede it with a backslash.  Setting an option requires an extra
	" backslash before a space and a backslash.  See also
	" |option-backslash|.  For example: >
	"     :set guifont=Screen15,\ 7x13,font\\,with\\,commas
  if g:os == "Darwin"
    set guifont=JetBrainsMono\ Nerd\ Font\ Mono\:h16
    set guifontwide=LXGW\ WenKai\ Mono\:h16
    " enable python3 support for neovim
    " :echo has('python3')
    " 1: enable, 0: disable
    let g:python3_host_prog="~/anaconda3/bin/python"
    " set guifont=Fira\ Mono:h12
  elseif g:os == "Linux"
    set guifont=JetBrainsMono\ Nerd\ Font\ Mono\ 16
    " enable python3 support for neovim
    " :echo has('python3')
    " 1: enable, 0: disable
    let g:python3_host_prog=system("which python")
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

" nvim: ~/.config/nvim/init.vim -> git repo
" vim: ~/.vimrc -> ~/.config/nvim/init.vim
" expand and resolve symbolic links
if has('nvim')
  let g:vim_config_dir=resolve(stdpath('config')) . "/"
else
  let g:vim_config_dir=fnamemodify(resolve(expand('<sfile>:p')),":h") . "/"
endif 
let s:packages=g:vim_config_dir . "configs/init-packages.vim"
execute "source " . s:packages

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

" cursor style {{{
" Nvim does not have special `t_XX` options nor <t_XX> keycodes to configure
" terminal capabilities. Instead Nvim treats the terminal as any other UI,
" e.g. 'guicursor' sets the terminal cursor style if possible.
if has('nvim')
  augroup reset_nvim_cursor
    autocmd! 
    autocmd VimEnter,VimResume * set guicursor=n-v-c:block,i-ci-ve:ver25,r-cr:hor20,o:hor50
		  \,a:blinkwait700-blinkoff400-blinkon250-Cursor/lCursor
		  \,sm:block-blinkwait175-blinkoff150-blinkon175
  augroup END "reset_nvim_cursor
else
  " vim
  " CSI code for cursors
  " CSI Ps SP q
  " Set cursor style (DECSCUSR), VT520.
  "   Ps = 0  -> blinking block.
  "   Ps = 1  -> blinking block (default).
  "   Ps = 2  -> steady block.
  "   Ps = 3  -> blinking underline.
  "   Ps = 4  -> steady underline.
  "   Ps = 5  -> blinking bar (xterm).
  "   Ps = 6  -> steady bar (xterm).
  let &t_SI .= "\e[5 q" " SI = INSERT mode
  let &t_SR .= "\e[3 q" " SR = REPLACE mode
  let &t_EI .= "\e[1 q" " EI = NORMAL mode (ELSE)
  " cursor style in terminal emulator
  let &t_Cs = "\e[4:3m"
  let &t_Ce = "\e[4:0m"
  hi SpellBad guisp=red gui=undercurl guifg=NONE guibg=NONE ctermfg=NONE ctermbg=NONE term=underline cterm=undercurl ctermul=red
  hi SpellCap guisp=yellow gui=undercurl guifg=NONE guibg=NONE ctermfg=NONE ctermbg=NONE term=underline cterm=undercurl ctermul=yellow
  " Initialize cursor shape/color on startup
  augroup reset_cursor_on_enter
    autocmd!
    autocmd VimEnter * normal! :startinsert :stopinsert
    autocmd VimEnter * redraw!
  augroup END
  " If you are a neovim user then you will need to install a terminfo file that tells neovim about this support.
  " see https://wezfurlong.org/wezterm/faq.html#how-do-i-enable-undercurl-curly-underlines
endif
" }}}

if exists('#Dracula')
  let g:dracula_bold = 1
  let g:dracula_italic = 1
  let g:dracula_underline = 1
endif

let g:gruvbox_italic=1
colorscheme gruvbox
" highlight Comment cterm=italic gui=italic
if !has('nvim')
  set background=dark
endif

" AUTOCMD GROUP {{{
" VimScript  {{{
function! s:VimAbbrev()
iabbr nnp nnoremap
iabbr vnp vnoremap
iabbr cnp cnoremap
iabbr xnp xnoremap
iabbr onp onoremap
iabbr tnp tnoremap
endfunction

  augroup filetype_vim
    autocmd!          | " Deletes all auto-commands in the current group
    autocmd FileType vim setlocal foldmethod=marker
    autocmd FileType vim call s:VimAbbrev()
    autocmd BufWritePost $MYVIMRC source $MYVIMRC
    " force equalizing window
    autocmd VimResized * tabdo wincmd =
  augroup END
  " }}}

  augroup filetype_misc
    autocmd!
    " ShellScript {{{
    autocmd FileType sh setlocal foldmethod=marker
    autocmd FileType zsh setlocal foldmethod=marker
    autocmd BufWritePost *.zsh set filetype=zsh
    autocmd BufWritePost *.sh set filetype=sh
    " }}}
  " git {{{
    autocmd  BufNewFile,BufRead *.gitignore set filetype=conf
    "}}}
    " tmux {{{
    autocmd FileType tmux setlocal foldmethod=marker
    " }}}
  " yaml {{{
    autocmd FileType yaml setlocal foldmethod=indent
  " }}}
  " c/cpp/objective-c {{{
    autocmd BufNewFile,BufRead,BufWritePost *.m set filetype=objc
    autocmd BufNewFile,BufRead,BufWritePost *.mm set filetype=objcpp
    autocmd BufNewFile,BufRead,BufWritePost *.cc set filetype=cpp
    autocmd BufNewFile,BufRead,BufWritePost *.c set filetype=c
    " }}}
  augroup END


  " python {{{
  aug ft_python
    autocmd!          | " Deletes all auto-commands in the current group
    " ftype/python.vim overwrites this
    autocmd FileType python 
          \ setlocal expandtab smarttab nosmartindent
          \ | setlocal tabstop=4 softtabstop=4 shiftwidth=4 textwidth=80

  aug end
" }}}

" golang  {{{
  aug ft_golang
    autocmd!         | " Deletes all auto-commands in the current group
    autocmd FileType go setlocal ts=4 sts=4 sw=4 noexpandtab
    autocmd BufNewFile,BufRead *.go setlocal foldmethod=syntax foldlevel=3
  aug end
  " }}}

  " markdown {{{
  augroup ft_md
    autocmd FileType markdown setlocal spell spelllang=en_us
    autocmd FileType markdown setlocal dictionary=/usr/share/dict/words
  augroup END "ft_md
  " }}}

" The following autocommand will cause the quickfix window to open after any grep invocation:
autocmd QuickFixCmdPost *grep* cwindow
" }}}

" ABBREVIATION {{{
" gloabal common abbreviations
iabbrev adn and
iabbrev waht what
iabbrev tehn then
iabbrev ture true
" }}}
