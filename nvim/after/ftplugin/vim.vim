  function! s:VimAbbrev()
  iabbr nnp nnoremap
  iabbr vnp vnoremap
  iabbr cnp cnoremap
  iabbr xnp xnoremap
  iabbr onp onoremap
  iabbr tnp tnoremap
  endfunction

  augroup filetype_vim
    autocmd!          | " Deletes all auto-commands in the current group
    autocmd FileType vim setlocal foldmethod=marker
    autocmd FileType vim call s:VimAbbrev()
    autocmd BufWritePost $MYVIMRC source $MYVIMRC
    " force equalizing window
    autocmd VimResized * tabdo wincmd =
  augroup END
