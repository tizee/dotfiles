if exists('loaded_eol_vim') || &cp
  finish
endif
let g:loaded_eol_vim = 1

" highlight end of line in files
autocmd Syntax * syn match ErrorMsg /\s\+$\| \+\ze\t/
