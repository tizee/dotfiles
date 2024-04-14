" see :help gitgutter

" Determines whether or not to show signs.
let g:gitgutter_signs=1

" Determines whether or not to show line highlights.
let g:gitgutter_highlight_lines=0

" Determines whether or not to show line number highlights.
let g:gitgutter_highlight_linenrs=1

" Sets the maximum number of signs to show in a buffer.
" Default: 500 (Vim < 8.1.0614, Neovim < 0.4.0) -1 (otherwise)
let g:gitgutter_max_signs=500

" Sets the |sign-priority| gitgutter assigns to its signs.
let g:gitgutter_sign_priority=10

" Determines whether gitgutter preserves non-gitgutter signs. When 1, gitgutter
" will not preserve non-gitgutter signs.
let g:gitgutter_sign_allow_clobber=0

let g:gitgutter_sign_added              = '┃'
let g:gitgutter_sign_modified           = '┃'
let g:gitgutter_sign_removed            = '┃'
let g:gitgutter_sign_removed_first_line = '‾'
let g:gitgutter_sign_removed_above_and_below = '_'
let g:gitgutter_sign_modified_removed   = '~'

" Only applies to existing GitGutter* highlight groups.
let g:gitgutter_set_sign_backgrounds=0

" update the signs when you save a file
autocmd BufWritePost * GitGutter
