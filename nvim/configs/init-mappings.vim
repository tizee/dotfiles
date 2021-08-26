" VIM COMPATIBLE KEY MAPPING {{{

nnoremap <space> <nop>
let mapleader = " "
let g:mapleader = " "
let maplocalleader = "\\"
" https://github.com/johndgiese/dotvim/issues/4
" https://vimhelp.appspot.com/term.txt.html#xterm-bracketed-paste
" Fix paste bug triggered by the above inoremaps
set t_BE=

" Normal Mode {{{

" virmrc/init.vim
" TODO: check existence
nnoremap <leader>ev :tabe $MYVIMRC<cr>
nnoremap <leader>sv :source $MYVIMRC<cr>

" disable arrow
noremap <Down> <nop>
noremap <Left> <nop>
noremap <Right> <nop>
noremap <Up> <nop>

" quick save
nnoremap S :w<cr>
" quick exit
nnoremap Q :q<cr>

" join lines without adding spaces
nnoremap J gJ
" clone current line
noremap Y y$

" termianl shortcut
" create terminal window then move it to the very bottom
function! s:create_terminal_panel()
  let t:terminal_name="terminal_buffer"
  silent! execute "bo split new"
  silent! execute "edit " . t:termianl_name
  silent! execute "resize 10"
  silent! execute "bo terminal"
endfunction
nnoremap <leader>t :call<SID>create_terminal_panel()<CR>

" enter command mode quickly
nnoremap ; :

" copy to/paste from system clipboard
nnoremap <leader>y "+y
nnoremap <leader>p "+p

" clear heighlight
nnoremap <silent><leader><CR> :noh<CR>


" Press ` to change case (instead of ~)
nnoremap ` ~

" Quick Replace
nnoremap <leader>R :%s//g<left><left>

" Quick paste default copy register in command mode
nnoremap <leader>V :<C-R>"

" Quick search word under cursor using help
nnoremap <leader>? :help <C-R><C-W>

" tab navigation
nnoremap <leader>1 1gt
nnoremap <leader>2 2gt
nnoremap <leader>3 3gt
nnoremap <leader>4 4gt
nnoremap <leader>5 5gt

function! s:open_url() abort
  let url = expand('<cfile>')
  if has('macunix') && executable('open')
    call system('open '.url)
    return
  elseif executable('xdg-open')
    call system('xdg-open '.url)
    return
  elseif has('win32') || has('win64')
    call system('cmd /c start "" /b '. substitute(url, '&', '^&', 'g'))
    if v:shell_error
      echohl Error | echom 'Failed to open '.url | echohl None
      return
    endif
  endif
endfunction

" alternative for broken netrwBrowseX
" TODO: figure what happens to netrw's gx
nmap <silent> <Plug>(gx-open-url) :call <SID>open_url()<CR>
nmap <silent> gx <Plug>(gx-open-url)

" }}}

" Insert Mode {{{
" uppercase inner word
inoremap <c-u> <esc>viwUi<esc>
inoremap ;; <ESC>
inoremap <ESC> <nop>
if empty($TMUX)
  inoremap <silent> <S-up> <ESC><C-u>i
  inoremap <silent> <S-down> <ESC><C-d>i
  inoremap <silent> <S-left> <ESC>I
  inoremap <silent> <S-right> <ESC>A
endif

inoremap [ []<left>
inoremap ( ()<left>
inoremap { {}<left>

" }}}

" Visual Mode {{{
vnoremap \ U
"inoremap <c-d> <esc>ddi

" quick replace
vnoremap <leader>R :s//g<left><left>
" quick search
" vnoremap // y/\V<C-r>=escape(@",'/\')<CR><CR>
" quick search all non-ascii characters 
" vnoremap <leader>a :s/\([^\u0020-\u0070]*\)/\1/
" quick search all non-ascii characters (including control characters) 
" vnoremap <leader>A :s/\([^\u0000-\u0070]*\)/\1/

" copy to/paste from system clipboard
vnoremap <leader>p "+p
vnoremap <leader>y "+y
" quick sort
vnoremap <leader>s :sort<CR>

" 
" }}}

" Command Mode {{{

" 
" }}}

" Terminal Mode {{{
if has("nvim")
  autocmd TermOpen * setlocal nonu nornu
  tnoremap <m-H> <nop>
  tnoremap <m-J> <nop>
  tnoremap <m-K> <nop>
  tnoremap <m-L> <nop>
  " simulate i_CTRL-R
  tnoremap <Esc> <C-\><C-n>
endif 

" }}}

" }}}

" vim:ft=vim
