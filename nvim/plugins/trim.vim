if exists('loaded_trim_vim') || &cp || v:version < 700
  finish
endif
let g:loaded_trim_vim = 1

function! s:TrimEOLSpace()
  call feedkeys(":\<C-U>%s/ *$//\<CR>")
  setl nohlsearch
endfunction

nnoremap <silent> <Plug>(trim-eol-spaces) :call <SID>TrimEOLSpace()<CR>

command! -nargs=0 TrimEOLSpace call <SID>TrimEOLSpace()
