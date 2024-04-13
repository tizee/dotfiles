" File: file.vim
" Author: tizee
" Email: 33030965+tizee@users.noreply.github.com
" Description: file related plugin

if exists('loaded_file_vim')
  finish
endif
let g:loaded_file_vim = 1

let g:large_file_size = get(g:,"large_file_size", 1024 * 1024 * 2) " 2Mb is a large file for me

function! s:change_opts()
  setl binary
  setl ei=FileType " ignore FileType event
  let l:msg="You have opened a file LAREGER than " . (g:large_file_size / (1024 * 1024)) . " MB"
  echohl WarningMsg | echom l:msg | echohl None
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

" created time
let s:stat_cmd="stat"
function! s:get_file_time(path)
  " %w created time
  " %x accessed time
  " %y modified time
  " %z status changed time
  let cmd = s:stat_cmd . " --printf='created time':%w\\\\n'accesed time':%x\\\\n'modified time':%y\\\\n'status changed time':%z " . a:path
  let command_output= system(cmd)
  " insert below
  execute ":put =command_output"
endfunction

command! -nargs=0 CurrentFileTime call <SID>get_file_time(expand("%"))

