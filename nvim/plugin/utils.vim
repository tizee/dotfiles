if exists('loaded_utils_vim') || &cp || v:version < 700
  finish
endif
let g:loaded_utils_vim = 1
" helpers

" [template name, default name]
function! g:UtilFilename(...)
  " current buffer name
  let l:filename= expand('%:t:r')
  " no name then use default name
  if filename == ''
    return a:0 == 2 ? a:2 : ''
  endif
  return !a:0 || a:1== '' ? filename : substitute(a:1, '$1', filename, 'g')
  " body
endfunction

highlight CustomPhysical guifg=White guibg=Red ctermfg=White ctermbg=Red cterm=bold
highlight CustomScreen guifg=Black guibg=Green ctermfg=Black ctermbg=Green cterm=bold
function! ToggleLineMovement()
    if maparg('j', 'n') ==# 'gj'
        nunmap j
        nunmap k
        nunmap 0
        nunmap $
        nunmap ^
        vunmap j
        vunmap k
        vunmap 0
        vunmap $
        vunmap ^
        echohl CustomPhysical
        echo "Line movement: PHYSICAL (normal j/k)"
        echohl None
    else
        " physical based
        nnoremap j gj
        nnoremap k gk
        nnoremap 0 g0
        nnoremap $ g$
        nnoremap ^ g^
        vnoremap j gj
        vnoremap k gk
        vnoremap 0 g0
        vnoremap $ g$
        vnoremap ^ g^
        echohl CustomScreen
        echo "Line movement: SCREEN (gj/gk)"
        echohl None
    endif
endfunction

command! ToggleLineMovement call ToggleLineMovement()
