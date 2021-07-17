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
" start screen
Plug 'mhinz/vim-startify'
" eye candy
if has('nvim')
Plug 'nvim-treesitter/nvim-treesitter', {'do': ':TSUpdate'}  " We recommend updating the parsers on update
endif
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
Plug 'dracula/vim', { 'as': 'dracula' }
" Recording
Plug 'wakatime/vim-wakatime'
" LSP + nvim nodejs ABI for ts plugins
Plug 'neoclide/coc.nvim', {'branch': 'release'}
" keyboard layout
Plug 'tizee/keyboard.vim'
" golang
Plug 'fatih/vim-go', { 'do': ':GoUpdateBinaries' }
" local vim plugin
Plug '~/dev/grepo_vim/goyo.vim'
Plug '~/dev/grepo_vim/vim-gh-line'
" my local plugin prototypes
Plug '~/dev/grepo_vim/tz-vim-packages/md-table.vim'
Plug '~/dev/grepo_vim/tz-vim-packages/moline.vim'
Plug '~/dev/grepo_vim/tz-vim-packages/unicode.vim'
"Plug '~/dev/grepo_vim/tz-vim-packages/keyboard.vim'
Plug '~/dev/grepo_vim/tz-vim-packages/boilerplate.vim'
Plug '~/dev/grepo_vim/indentLine'
" language checker
Plug '~/dev/grepo_vim/vim-LanguageTool'
" beancount
Plug '~/dev/grepo_vim/vim-beancount'
" }}}
call plug#end()

" simple plugins
" configs/../plugins
let s:plugin_prefix=get(g:,'vim_config_dir') . "/plugins/"
let s:plugins=readdir(s:plugin_prefix)
for plugin_filename in s:plugins
  let plugin = s:plugin_prefix . plugin_filename
  execute "source " . plugin
endfor

" plugin prototype
set rtp+=~/dev/grepo_vim/tz-vim-packages/nodemodules.vim
set rtp+=~/dev/grepo_vim/tz-vim-packages/find-root.vim

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

autocmd BufWritePre *.go :silent call CocAction('runCommand', 'editor.action.organizeImport')
autocmd FileType go autocmd BufWritePre <buffer> Format
autocmd FileType go nmap gtj :CocCommand go.tags.add json<cr>
autocmd FileType go nmap gty :CocCommand go.tags.add yaml<cr>
autocmd FileType go nmap gtx :CocCommand go.tags.clear<cr>
" }}}

" PLUGIN CONFIGURATIONS {{{
" shit code
source ~/.config/nvim/configs/tz-languagetool.vim
source ~/.config/nvim/configs/tz-coc-config.vim
source ~/.config/nvim/configs/tz-nerdtree.vim
source ~/.config/nvim/configs/tz-vim-autoformat.vim
source ~/.config/nvim/configs/tz-lightline.vim
source ~/.config/nvim/configs/tz-utils.vim
source ~/.config/nvim/configs/tz-fzf.vim
source ~/.config/nvim/configs/tz-tabular.vim
source ~/.config/nvim/configs/tz-startify.vim
" source ~/.config/nvim/configs/tz-ultisnips.vim
source ~/.config/nvim/configs/tz-which-key.vim
source ~/.config/nvim/configs/tz-rainbow.vim
source ~/.config/nvim/configs/tz-keyboard.vim
source ~/.config/nvim/configs/tz-mappings.vim
source ~/.config/nvim/configs/tz-goyo.vim
" }}}

