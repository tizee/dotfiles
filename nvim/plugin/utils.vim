if exists('loaded_utils_vim') || &cp || v:version < 700
  finish
endif
let g:loaded_utils_vim = 1
" helpers

" [template name, default name]
function! g:UtilFilename(...)
  " current buffer name
  let l:filename= expand('%:t:r')
  " no name then use default name
  if filename == ''
    return a:0 == 2 ? a:2: ''
  endif
  return !a:0 || a:1== '' ? filename: substitute(a:1, '$1', filename, 'g')
  " body
endfunction
