if exists('loaded_redir_vim') || &cp || v:version < 700
  finish
endif
let g:loaded_redir_vim = 1

" Capture vim command output
" I think :read is enough for capturing shell command output
" https://vi.stackexchange.com/questions/8378/dump-the-output-of-internal-vim-command-into-buffer
command! -nargs=+ -complete=command Redir let s:reg = @@ | redir @"> | silent execute <q-args> | redir END | new | pu | 1,2d_ | let @@ = s:reg
