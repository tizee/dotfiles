" File: file.vim
" Author: tizee
" Email: 33030965+tizee@users.noreply.github.com
" Description: file timestamps plugin

if exists('loaded_file_vim')
  finish
endif
let g:loaded_file_vim = 1

let g:large_file_size = get(g:,"large_file_size", 1024 * 1024 * 5) " 5mb is a large file for me

function! s:change_opts()
  setl binary
  setl ei=FileType " ignore FileType event
  echo "You have opened a file LAREGER than " . (g:large_file_size / (1024 * 1024)) . " MB"
endfunction

function! s:handle_largefile(file)
  let f = getfsize(a:file)
  if f > g:large_file_size || f == -2
    autocmd file_vim VimEnter * call <SID>change_opts()
  endif
endfunction

function! s:handle_Chinese_file()
  setl formatoptions+=m
endfunction

augroup file_vim
  autocmd BufReadPre * call <SID>handle_largefile(expand("<afile>"))
  autocmd BufReadPre zh-* call <SID>handle_Chinese_file()
augroup END "file_vim

" TODO
function! s:get_file_time()
  " created time
  " modfied time
endfunction
