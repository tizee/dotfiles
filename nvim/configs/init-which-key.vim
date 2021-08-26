
autocmd! FileType which_key
autocmd  FileType which_key set laststatus=0 noshowmode noruler
  \| autocmd BufLeave <buffer> set laststatus=2 showmode ruler

" show available key bindings for <leader>
nnoremap <silent> <leader> :<c-u>WhichKey '<Space>'<CR>
" show available key bindings for -
nnoremap <silent> <leader>- :<c-u>WhichKey '-'<CR>
