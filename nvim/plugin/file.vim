" File: file.vim
" Author: tizee
" Email: 33030965+tizee@users.noreply.github.com
" Description: file related plugin

if exists('loaded_file_vim')
  finish
endif
let g:loaded_file_vim = 1

let g:large_file_size = get(g:,"large_file_size", 1024 * 1024 * 2) " 2Mb is a large file for me

function! s:print(msg)
  echohl MoreMsg | echom a:msg | echohl None
endfunction

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

function! s:handle_chinese_file()
  setl formatoptions+=m
endfunction

augroup file_vim
  autocmd BufReadPre * call <SID>handle_largefile(expand("<afile>"))
  autocmd BufReadPre zh-* call <SID>handle_chinese_file()
augroup END "file_vim

" inspired by yuchanns/ccr.nvim
" created time
function! s:get_file_time(path)
  if has("linux")
    " %w created time
    " %x accessed time
    " %y modified time
    " %z status changed time
    let cmd = "stat --printf='created time':%w\\\\n'accesed time':%x\\\\n'modified time':%y\\\\n'status changed time':%z " . a:path
    let command_output= system(cmd)
    " insert below current line
    execute ":put =command_output"
  elseif has("mac")
    let command_output=system(["stat","-f", "Created time:%SB%naccesed time:%Sa%nModified time:%Sm%nStatus changed time:%Sc%n", "-t '%F %R'", a:path])
    " insert below current line
    execute ":put =command_output"
  end
endfunction


function! s:get_file_relative_path()
  let output=expand("%:~:.")
  call s:print("copy to unamed register: " . output)
  let @"=output
endfunction

function! s:get_file_relative_path_with_line()
  let output=expand("%:~:.") . ":" . line(".")
  call s:print("copy to unamed register: " . output)
  let @"=output
endfunction

" filename-modifiers
" :~ Reduce file name to be relative to the home directory, if
" :. Reduce file name to be relative to current directory
function! s:get_file_relative_path_with_line()
  let output=expand("%:~:.") . ":" . line(".")
  call s:print("copy to unamed register: " . output)
  let @"=output
endfunction

function! s:get_file_absolute_path_with_line()
  let output=expand("%:p") . ":" . line(".")
  call s:print("copy to unamed register: " . output)
  let @"=output
endfunction

function! s:get_file_absolute_path()
  let output=expand("%:p")
  call s:print("copy to unamed register: " . output)
  let @"=output
endfunction

function! s:get_file_dir_path()
  let output=expand("%:p:h")
  call s:print("copy to unamed register: " . output)
  let @"=output
endfunction

function! s:get_file_name()
  let output=expand("%:t")
  call s:print("copy to unamed register: " . output)
  let @"=output
endfunction

function! s:get_file_extension()
  let output=expand("%:e")
  call s:print("copy to unamed register: " . output)
  let @"=output
endfunction

function! s:get_file_name_without_extension()
  let output=expand("%:r")
  call s:print("copy to unamed register: " . output)
  let @"=output
endfunction

command! -nargs=0 FileTime call <SID>get_file_time(expand("%"))
command! -nargs=0 FileRelPathLineNumber call <SID>get_file_relative_path_with_line()
command! -nargs=0 FileRelPath call <SID>get_file_relative_path()
command! -nargs=0 FileAbsPath call <SID>get_file_absolute_path()
command! -nargs=0 FileAbsPathLineNumber call <SID>get_file_absolute_path_with_line()
command! -nargs=0 FileDirPath call <SID>get_file_dir_path()
command! -nargs=0 FileName call <SID>get_file_name()
command! -nargs=0 FileExtension call <SID>get_file_extension()
command! -nargs=0 FileNameWithoutExtention call <SID>get_file_name_without_extension()
