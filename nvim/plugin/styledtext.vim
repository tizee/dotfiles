" File: styledtext.vim
" Description: vim plugin for 𝕤𝕥𝕪𝕝𝕖𝕕𝕥𝕖𝕩𝕥
" Author: tizee
" Email: 33030965+tizee@users.noreply.github.com
" Github: https://github.com/tizee/dotfiles

if exists('loaded_styledtext_vim') || &cp || v:version < 700
  finish
endif
let g:loaded_styledtext_vim = 1

function! s:insert_after_cursor(text)
  let cur_line_num = line('.')
  let cur_col_num = col('.')
  let cur_line = getline('.')
  " new line: text_before_cursor + inserted_text + text_after_cursor
  let new_line = strpart(cur_line, 0, cur_col_num - 1) . a:text . strpart(cur_line, cur_col_num - 1)
  call setline(cur_line_num, new_line)
  " set cursor position
  call cursor(cur_line_num, cur_col_num + strlen(a:text))
endfunction

function! s:styledtext_echo_doublestruck(msg)
  call s:styledtext_echo(a:msg, "bold", "doublestruck")
endfunction

function! s:styledtext_echo_fraktur(msg)
  call s:styledtext_echo(a:msg, "bold", "fraktur")
endfunction

function! s:styledtext_echo_script(msg)
  call s:styledtext_echo(a:msg, "bold", "script")
endfunction

function! s:styledtext_echo_mono(msg)
  call s:styledtext_echo(a:msg, "normal", "monospace")
endfunction

function! s:styledtext_echo(msg, style, type)
  if executable("styledtext")
    let output=trim(system(["styledtext", "--letter-style", a:style, "--letter-type", a:type, a:msg]))
    call s:insert_after_cursor(l:output)
  else
    echoerr "styledtext not found"
  endif
endfunction

command! -nargs=1 StyledTextMono call <SID>styledtext_echo_mono("<args>")
command! -nargs=1 StyledTextScript call <SID>styledtext_echo_script("<args>")
command! -nargs=1 StyledTextFraktur call <SID>styledtext_echo_fraktur("<args>")
command! -nargs=1 StyledTextDoubleStruck call <SID>styledtext_echo_doublestruck("<args>")
