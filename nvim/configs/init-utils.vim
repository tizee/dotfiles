" SCRIPTS {{{

function! s:openURL() abort
  let url = expand('<cfile>')
  if has('macunix') && executable('open')
    call system('open '.url)
    return
  elseif executable('xdg-open')
    call system('xdg-open '.url)
    return
  elseif has('win32') || has('win64')
    call system('cmd /c start "" /b '. substitute(url, '&', '^&', 'g'))
    if v:shell_error
      echohl Error | echom 'Failed to open '.url | echohl None
      return
    endif
  endif
endfunction

" alternative for broken netrwBrowseX
nnoremap <silent> <Plug>(gx-open-url) :call <SID>openURL()<CR>
nnoremap <silent> gx <Plug>(gx-open-url)

" }}}
