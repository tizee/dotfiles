if exists('loaded_trim_vim') || &cp || v:version < 700
  finish
endif
let g:loaded_trim_vim = 1

function! s:TrimEOLSpaces()
  call feedkeys(":\<C-U>%s/ *$//\<CR>")
  setl nohlsearch
endfunction

nnoremap <silent> <Plug>(trim-eol-spaces) :call <SID>TrimEOLSpaces()<CR>

command! -nargs=0 TrimEOLSpaces call <SID>TrimEOLSpaces()

augroup TRIM_EOL_SPACES
  autocmd!
  autocmd BufWritePost * call <SID>TrimEOLSpaces()
augroup END

