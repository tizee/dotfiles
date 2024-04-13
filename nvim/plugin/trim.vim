if exists('loaded_trim_vim') || &cp || v:version < 700
  finish
endif
let g:loaded_trim_vim = 1
let g:disable_trialing_spaces = v:false
let g:disable_trim_msg = v:false

function s:toggleTrimEOLSpaces() abort
  let g:disable_trialing_spaces = !g:disable_trialing_spaces
  echohl MoreMsg | echomsg "disable: " . g:disable_trialing_spaces | echohl None
endfunction

function s:toggleTrimMsg() abort
  let g:disable_trim_msg = !g:disable_trim_msg
  echohl MoreMsg | echomsg "disable trim msg: " . g:disable_trim_msg | echohl None
endfunction

function! s:TrimEOLSpaces()
  if g:disable_trialing_spaces
   return
  endif
  if !g:disable_trim_msg
    echohl MoreMsg
      echomsg "Trim all trailing spaces before eol"
    echohl None
    while !getchar()
    endwhile
  endif

  let saved_cursor=getcurpos()
  let saved_gdefault = &gdefault
  let saved_hlsearch = &hlsearch
  try
      set nogdefault
      set nohlsearch
      " call feedkeys(":\<C-U>%s/ *$//\<CR>")
      " remove all trailing spaces or tabs
      silent! %s/\s\+$
  finally
      let &gdefault = saved_gdefault
      let &hlsearch = saved_hlsearch
      call setpos('.',saved_cursor)
  endtry
endfunction

nnoremap <silent> <Plug>(trim-eol-spaces) :call <SID>TrimEOLSpaces()<CR>

command! -nargs=0 TrimEOLSpaces call <SID>TrimEOLSpaces()
command! -nargs=0 TrimEOLSpacesToggle call <SID>toggleTrimEOLSpaces()
command! -nargs=0 TrimEOLSpacesMsgToggle call <SID>toggleTrimMsg()

augroup TRIM_EOL_SPACES
  autocmd!
  autocmd BufWritePre * call <SID>TrimEOLSpaces()
augroup END

