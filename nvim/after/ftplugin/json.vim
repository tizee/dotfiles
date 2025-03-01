" use following command to check the last script that modified the setting
" :verbose set conceallevel?
set conceallevel=0 " json quote
" plugin IndentLine
" A. Use let g:vim_json_syntax_conceal = 0 or run :IndentLinesDisable
let g:vim_json_syntax_conceal = 0

autocmd FileType json setlocal foldmethod=syntax foldlevel=99
