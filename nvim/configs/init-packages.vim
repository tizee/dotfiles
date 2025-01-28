" File: init-packages.vim
" Author: tizee
" Email: 33030965+tizee@users.noreply.github.com
" Description: package list
scriptencoding utf-8
" run-once
if exists('loaded_tz_packages') || &cp || v:version < 700
  finish
endif
let g:loaded_tz_packages= 1

call plug#begin('~/.vim/plugged')
" Plugin list {{{

" ===> nvim plugin {{{
if has('nvim')
  " -- treesitter highlighter
  Plug 'nvim-treesitter/nvim-treesitter', {'do': ':TSUpdate'}
  " treesitter-based textojbects
  Plug 'nvim-treesitter/nvim-treesitter-textobjects'
  " rename
  Plug 'nvim-treesitter/nvim-treesitter-refactor'
endif
"  }}}

" ===> Formatter/Linter {{{
" Chinese formatter
Plug 'hotoo/pangu.vim'
" -- editorconfig
Plug 'editorconfig/editorconfig-vim'
" open-source grammar checker (alternative of Grammarly)
" Plug 'dpelle/vim-LanguageTool'
" }}}

" ===> language Syntax highlighting or more {{{
" -- pug/Jade
Plug 'digitaltoad/vim-pug'
" -- nginx syntax
Plug 'chr4/nginx.vim'
" -- toml
Plug 'cespare/vim-toml'
" -- glsl
Plug 'tikhomirov/vim-glsl' " OpenGL shader language
" -- colored parentheses
Plug 'luochen1990/rainbow'
" -- jsonc
Plug 'kevinoid/vim-jsonc'
" ziglang
Plug 'ziglang/zig.vim'
" beancount
Plug 'nathangrigg/vim-beancount'
" cmake
Plug 'pboettch/vim-cmake-syntax'
" }}}

" ===> Enhanced Editing Workflow {{{
" -- commenting mappings
Plug 'tpope/vim-commentary'
" -- edit surrounding symbol pairs
Plug 'tpope/vim-surround'
" -- quick navigation
Plug 'easymotion/vim-easymotion'
" -- quick alignment
Plug 'godlygeek/tabular'
" -- display leader key mappings
Plug 'liuchengxu/vim-which-key'
" -- convert color values to different formats
Plug 'amadeus/vim-convert-color-to'
" -- mappings for different keyboard layout
Plug 'tizee/keyboard.vim'
" -- git blame / open git repo url
Plug 'tizee/vim-gh-line'
" -- search package in nodemodules in yarn/npm managed project
Plug 'tizee/nodemodules.vim'
" -- generate markdown table
Plug 'tizee/md-table.vim'
" -- convert rgb hex string to r,g,b
Plug 'tizee/hex2rgb.vim'
" -- convert unicode value to corresponding unicode character
Plug 'tizee/unicode.vim'
" -- copy text to the system clipboard from anywhere using the ANSI OSC52 sequence
Plug 'ojroques/vim-oscyank'
if has('nvim')
  " -- swap values in ternary expressions
  Plug 'xlboy/swap-ternary.nvim'
endif
" }}}
"
" ===> Git plugins {{{
" -- git operations commands
Plug 'tpope/vim-fugitive'
" -- display git gutter signs in signcolumn
Plug 'airblade/vim-gitgutter' " git
" }}}

" ===> Snippets {{{
" -- ultisnips
Plug 'SirVer/ultisnips'
" }}}

" ===> UI {{{
" -- statusline
Plug 'tizee/moline.vim'
Plug 'tizee/moline.vim'
" -- tabline
Plug 'tizee/tabline.vim'
" -- Nerdfont icons
Plug 'tizee/vim-devicons'
" -- colorscheme
Plug 'tizee/gruvbox.vim'
" -- start screen
Plug 'mhinz/vim-startify'
" -- zen mode
Plug 'junegunn/goyo.vim'
" Indent
" -- Show vertical lines for indent with conceal feature
Plug 'Yggdroot/indentLine'

" }}}

" ===> Debugging {{{
" Plug 'puremourning/vimspector'

" }}}

" ===> File manager {{{
" -- nerdtree
Plug 'preservim/nerdtree'
" -- nerdtree with git status support
Plug 'tizee/nerdtree-git-plugin'
" -- vifm
" Plug 'vifm/vifm.vim'

" }}}

" ===> Editor Statistics {{{
" -- Wakatime
Plug 'wakatime/vim-wakatime'
" }}}

" ===> Vim Functions {{{
" -- find file path until root
Plug 'tizee/findfirst.vim', {'branch': 'master'}
" }}}

" ===> LSP {{{
" -- LSP + nvim nodejs ABI for ts plugins
" -- use my forked of coc.nvim
Plug 'tizee/coc.nvim', {'branch': 'master', 'do': 'pnpm run install --frozen-lockfile'}
" -- lua lsp: coc-stylua
" -- golang lsp: coc-go
" -- conflicts with coc-go
" Plug 'fatih/vim-go', { 'do': ':GoUpdateBinaries' }
" -- symbol tags
" it's the same as :CocOutline directly
Plug 'liuchengxu/vista.vim'
" }}}

" ===> AI {{{

" simple inline AI chat
Plug '~/projects/project-vim/tizee-plugins/llm.nvim'

" }}}

call plug#end()

" vim: foldmarker={{{,}}}:foldmethod=marker
