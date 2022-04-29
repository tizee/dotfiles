"
"  Enhanced split
"  A tool help split buffers accroding to paths. 
"
if exists('loaded_split_vim')
  finish
endif
let g:loaded_split_vim = 1

function! s:SplitUnderBufferPath(is_vertical,filename)
  " change the current directory for current buffer's directory
  let l:buffer_directory='%:p:h'
  execute ':lcd ' . l:buffer_directory
  if a:is_vertical == 1
    execute ':vsplit ' . a:filename
  else
    execute ':split ' . a:filename
  endif 
endfunction

" change working directory: lcd tcd
" create new buffer
" remove new buffer

command! -nargs=1 SplitBufferVertical call <SID>SplitUnderBufferPath(1,<f-args>)
command! -nargs=1 SplitBuffer call <SID>SplitUnderBufferPath(0,<f-args>)

cabbrev vbsplit SplitBufferVertical
cabbrev bsplit SplitBuffer
