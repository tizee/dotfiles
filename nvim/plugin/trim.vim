if exists('loaded_trim_vim') || &cp || v:version < 700
  finish
endif
let g:loaded_trim_vim = 1
let g:disable_trialing_spaces = v:false

function s:toggleTrimEOLSpaces() abort
  let g:disable_trialing_spaces = !g:disable_trialing_spaces
  echohl WarningMsg | echomsg "disable: " . g:disable_trialing_spaces | echohl None
endfunction

function! s:TrimEOLSpaces()
  if g:disable_trialing_spaces
   return
  endif
  echohl WarningMsg | echomsg "Trim all trailing spaces before eol" | echohl None

  let saved_cursor=getcurpos()
  let saved_gdefault = &gdefault
  let saved_hlsearch = &hlsearch
  try
      set nogdefault
      set nohlsearch
      " call feedkeys(":\<C-U>%s/ *$//\<CR>")
      silent! %s/\s\+$
  finally
      let &gdefault = saved_gdefault
      let &hlsearch= saved_hlsearch
      call setpos('.',saved_cursor)
  endtry
endfunction

nnoremap <silent> <Plug>(trim-eol-spaces) :call <SID>TrimEOLSpaces()<CR>

command! -nargs=0 TrimEOLSpaces call <SID>TrimEOLSpaces()
command! -nargs=0 TrimEOLSpacesToggle call <SID>toggleTrimEOLSpaces()

augroup TRIM_EOL_SPACES
  autocmd!
  autocmd BufWritePre * call <SID>TrimEOLSpaces()
augroup END

