" File: init-packages.vim
" Author: tizee
" Email: 33030965+tizee@users.noreply.github.com
" Description: 

scriptencoding utf-8
call plug#begin('~/.vim/plugged')
" plugin list {{{
" text alignment
" Plug 'SirVer/ultisnips'
Plug 'digitaltoad/vim-pug'
Plug 'chr4/nginx.vim'
Plug 'cespare/vim-toml'
Plug 'godlygeek/tabular'
" colored parentheses
Plug 'luochen1990/rainbow'
" eye candy
if has('nvim')
Plug 'nvim-treesitter/nvim-treesitter', {'do': ':TSUpdate'}  " We recommend updating the parsers on update
endif
" debugger
Plug 'puremourning/vimspector'
" Plug 'itchyny/lightline.vim'
Plug 'ryanoasis/vim-devicons' " icons
Plug 'airblade/vim-gitgutter' " git
" key mapping helper
Plug 'liuchengxu/vim-which-key'
Plug 'kevinoid/vim-jsonc'
" file manager
Plug 'preservim/nerdtree'
Plug '~/dev/grepo_vim/nerdtree-git-plugin'
" highlighting {{{
" Plug 'HerringtonDarkholme/yats.vim'
" Plug 'yuezk/vim-js'
" Plug 'maxmellon/vim-jsx-pretty'
Plug 'tikhomirov/vim-glsl' " OpenGL shader language
" }}}
" formater
Plug 'Chiel92/vim-autoformat'
" Editing
Plug 'tpope/vim-commentary' " commentary tool
Plug 'tpope/vim-surround'
Plug 'tpope/vim-fugitive'
Plug 'easymotion/vim-easymotion'
" Searching
Plug '/usr/local/opt/fzf'
" Plug 'dracula/vim', { 'as': 'dracula' }
Plug 'morhetz/gruvbox'
" Recording
Plug 'wakatime/vim-wakatime'
" LSP + nvim nodejs ABI for ts plugins
Plug 'neoclide/coc.nvim', {'branch': 'release'}
" TODO: add function for retrieving lsp info
" Plug '~/dev/grepo_vim/coc.nvim'
" keyboard layout
" Plug 'tizee/keyboard.vim'
" convert rgb hex string to r,g,b
Plug 'tizee/hex2rgb.vim'
" convert unicode value to corresponding unicode character
Plug 'tizee/unicode.vim'
" Chinese format
Plug 'hotoo/pangu.vim'
" golang
" conflict with coc-go
" Plug 'fatih/vim-go', { 'do': ':GoUpdateBinaries' }
" local vim plugin
" start screen
Plug '~/dev/grepo_vim/vim-startify'
" zen mode
Plug '~/dev/grepo_vim/goyo.vim'
" git blame / open git repo url
Plug '~/dev/grepo_vim/vim-gh-line'
" copy text to the system clipboard from anywhere using the ANSI OSC52 sequence
Plug '~/dev/grepo_vim/vim-oscyank'
" my local plugin prototypes
Plug '~/dev/grepo_vim/tz-vim-packages/find-root.vim'
Plug '~/dev/grepo_vim/tz-vim-packages/nodemodules.vim'
Plug '~/dev/grepo_vim/tz-vim-packages/md-table.vim'
Plug '~/dev/grepo_vim/tz-vim-packages/moline.vim'
" Plug '~/dev/grepo_vim/tz-vim-packages/codecount.vim'
" local version fixed packages
Plug '~/dev/grepo_vim/indentLine'
" language checker
Plug '~/dev/grepo_vim/vim-LanguageTool'
" beancount
Plug '~/dev/grepo_vim/vim-beancount'
" osc-yank
Plug '~/dev/grepo_vim/vim-oscyank'
" vista for tags
" could use :CocOutline directly
Plug '~/dev/grepo_vim/vista.vim'
" Leaderf
Plug '~/dev/grepo_vim/LeaderF',{ 'do': ':LeaderfInstallCExtension' }

" }}}
call plug#end()
" vim-plug autocmd {{{

" open plugin help docs
function! s:plug_doc()
  let name = matchstr(getline('.'), '^- \zs\S\+\ze:')
  if has_key(g:plugs, name)
    for doc in split(globpath(g:plugs[name].dir, 'doc/*.txt'), '\n')
      execute 'tabe' doc
    endfor
  endif
endfunction

" open the GitHub URL for a plugin or a commit with the default browser
function! s:plug_gx()
  let line = getline('.')
  let sha  = matchstr(line, '^  \X*\zs\x\{7,9}\ze ')
  let name = empty(sha) ? matchstr(line, '^[-x+] \zs[^:]\+\ze:')
	\ : getline(search('^- .*:$', 'bn'))[2:-2]
  let uri  = get(get(g:plugs, name, {}), 'uri', '')
  if uri !~ 'github.com'
    return
  endif
  let repo = matchstr(uri, '[^:/]*/'.name)
  let url  = empty(sha) ? 'https://github.com/'.repo
	\ : printf('https://github.com/%s/commit/%s', repo, sha)
  call netrw#BrowseX(url, 0)
endfunction

augroup VimPlugPlus
  autocmd! FileType vim-plug nmap <buffer> ? <plug>(plug-preview)
  autocmd! FileType vim-plug nnoremap <buffer> <silent> <C-h> :call <sid>plug_doc()<cr>
  autocmd! FileType vim-plug nnoremap <buffer> <silent> <Tab> :call <sid>plug_gx()<cr>
augroup end
" }}}

if exists('loaded_tz_packages_vim') || &cp || v:version < 700
  finish
endif

let g:loaded_tz_packages_vim = 1
function! s:source_helper(name)
  let plugin_prefix=get(g:,'vim_config_dir') . a:name . "/"
  let plugins=readdir(plugin_prefix)
  for plugin_filename in plugins
    let plugin = plugin_prefix . plugin_filename
    if  plugin_filename ==# 'packages' || isdirectory(plugin)
      continue
    endif
    execute "source " . plugin
  endfor
endfunction

" plugin prototype
" coc extension local development
set rtp+=~/dev/grepo_vim/coc-packages/coc-flutter

call s:source_helper('plugins')
call s:source_helper('configs')
