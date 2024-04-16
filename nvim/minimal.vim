" vim: set foldmethod=marker foldlevel=0 nomodeline:

" use ":verbose set <option>?" to debug the source of option changes
" ========== vim basic options ========= {{{

let g:mapleader=" "

" highlight text line of cursor
set cursorline
" highlight text column of cursor
set nocursorcolumn

" workaround for italics
" https://rsapkf.xyz/blog/enabling-italics-vim-tmux
set t_ZH=[3m
set t_ZR=[23m

" Encoding
scriptencoding utf-8
set fileencodings=utf-8,cp936
set fileencoding=utf-8
set encoding=utf8

" backspace
" see :help bs
" Influences the working of <BS>, <Del>, CTRL-W and CTRL-U in Insert mode.
set bs=indent,eol,start


" whether to use Python 2 or 3
set pyx=3

" true color
"Use 24-bit (true-color) mode in Vim/Neovim when outside tmux.
"If you're using tmux version 2.2 or later, you can remove the outermost $TMUX check and use tmux's 24-bit color support
"(see < http://sunaku.github.io/tmux-24bit-color.html#usage > for more information.)
if (empty($TMUX))
  if (has("nvim"))
    "For Neovim 0.1.3 and 0.1.4 < https://github.com/neovim/neovim/pull/2198 >
    let $NVIM_TUI_ENABLE_TRUE_COLOR=1
  endif
  "For Neovim > 0.1.5 and Vim > patch 7.4.1799 < https://github.com/vim/vim/commit/61be73bb0f965a895bfb064ea3e55476ac175162 >
  "Based on Vim patch 7.4.1770 (`guicolors` option) < https://github.com/vim/vim/commit/8a633e3427b47286869aa4b96f2bfc1fe65b25cd >
  " < https://github.com/neovim/neovim/wiki/Following-HEAD#20160511 >
  if has("termguicolors")
    set termguicolors
  endif
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
" decrease memory usage
set bufhidden=wipe

"show command line
set showcmd
" Give more space for displaying messages.
set cmdheight=2

" opening message hit-enter
" Don't pass messages to |ins-completion-menu|.
set shortmess+=c

" TextEdit might fail if hidden is not set.
set hidden

" Having longer updatetime (default is 4000 ms = 4 s) leads to noticeable delays and poor user experience.
set updatetime=100
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
  " create backup
  silent !mkdir -p ~/.config/nvim/tmp/backup
  " create undo
  silent !mkdir -p ~/.config/nvim/tmp/undo
  "silent !mkdir -p ~/.config/nvim/tmp/sessions
  set backupdir=~/.config/nvim/tmp/backup,.
  set directory=~/.config/nvim/tmp/backup,.
else
  set viminfo='100,<10,@50,h,n$DATA_PATH/viminfo
  " Secure sensitive information, disable backup files in temp directories
  if exists('&backupskip')
    set backupskip+=/tmp/*,$TMPDIR/*,$TMP/*,$TEMP/*,*/shm/*,/private/var/*
    set backupskip+=.vault.vim
  endif
endif


" If sudo, disable vim swap/backup/undo/shada/viminfo writing
if $SUDO_USER !=# '' && $USER !=# $SUDO_USER
  \ && $HOME !=# expand('~'.$USER, 1)
  \ && $HOME ==# expand('~'.$SUDO_USER, 1)

" Backup and swap
" Some servers have issues with backup files, see coc.vim/#649.
" not createing swap file for new buffers
 set noswapfile
 set nowritebackup
 set nobackup
 set nowritebackup
 set noundofile
 if has('nvim')
  set shada="NONE"
 else
  set viminfo="NONE"
 endif
endif

" disable swap file
set noswapfile

" Don't pass messages to |ins-completion-menu|.
" set shortmess+=c

" Always show the signcolumn, otherwise it would shift the text each time
" diagnostics appear/become resolved.
if has("patch-8.1.1564")
  " Recently vim can merge signcolumn and number column into one
  set signcolumn=yes
else
  set signcolumn=yes:3
endif

" grepprg
set grepprg=rg\ --vimgrep
set grepformat=%f:%l:%c:%m


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
set numberwidth=4

" show line number relative to current line where cursor is
set norelativenumber

" check filetype
filetype on

" Indent {{{

" always use tab character
" set noexpandtab

" always use space by default
set expandtab

set shiftwidth=2

" Number of spaces that a <Tab> in the file counts for.
" shift indent
" set tabstop=8
set tabstop=2
set textwidth=120

" Number of spaces that a <Tab> counts for while performing editing
" operations, like inserting a <Tab> or using <BS>.
set softtabstop=2

" Do smart autoindenting when starting a new line.
set nosmartindent

" When on, a <Tab> in front of a line inserts blanks according to'shiftwidth'.
set nosmarttab
set noautoindent

" use spaces wtih < or > commands
" set autoindent
" }}}

" show match
set showmatch

" show command line
set showcmd
set cmdheight=2

" Better command-line completion
set wildmenu

" show cursor location
set ruler

" highlight searchs
set hlsearch
set incsearch


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

" tabline-settings {{{
" 0: never, 1: show when 2 or more tabs, 3: always
set showtabline=2

" }}}


" }}}
