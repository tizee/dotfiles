"====================
" goyo.vim
"====================
if exists('loaded_tzgoyo_vim') || &cp || v:version < 700
  finish
endif
let g:loaded_tzgoyo_vim = 1
" listchars
let s:saved_lcs=&lcs
function! s:on_goyo_enter()
  if exists('$TMUX')
    set lcs&vim
    silent !tmux set status off
  endif 
endfunction

function! s:on_goyo_leave()
  if exists('$TMUX')
    let &lcs=s:saved_lcs
    silent !tmux set status on
  endif 
endfunction

augroup TzGoyo
  autocmd!
  autocmd! User GoyoEnter nested call <SID>on_goyo_enter()
  autocmd! User GoyoLeave nested call <SID>on_goyo_leave()
augroup END "TzGoyo

nnoremap <leader>G :Goyo<CR>

" vim:set foldmethod=marker
