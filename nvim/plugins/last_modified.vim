" File: last_modified.vim
" Author: tizee
" Email: 33030965+tizee@users.noreply.github.com
" Github: 
" Description:
" Last Modified: Sun 2021-08-22 01:40:25 (+0800)

if exists('loaded_last_modified_vim') || &cp || v:version < 700
  finish
endif
let g:loaded_last_modified_vim = 1

let s:save_cpo = &cpo
set cpo&vim

let s:lastmod_suffix=''
let s:lastmod_line=6
let s:lastmod_format='%a %Y-%m-%d %H:%M:%S (%z)'
let s:lastmod_prefix='Last Modified:\s*'

function! s:UpdateTime()
      let save_cursor = getpos('.')
      let n = min([s:lastmod_line, line('$')])
      let timestamp = strftime(s:lastmod_format)
      echom "Update Last modified time " . timestamp
      " build up pattern
      let pat = s:lastmod_prefix .'\zs.*\ze' . s:lastmod_suffix
      let pat = substitute(pat, '%', '\%', 'g')
      let timestamp = substitute(timestamp, '%', '\%', 'g')
      keepjumps silent exe '1,'.n.'s%^.*'.pat.'.*$%'.timestamp.'%e'
      call histdel('search', -1)
      " restore cursor position
      call setpos('.', save_cursor)
endfunction

command! -bar UpdateMod call s:UpdateTime()
noremap <unique> <script> <Plug>LastModifiedUpdate <SID>Update

let &cpo = s:save_cpo
