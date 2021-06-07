" ========== lightline ========== {{{
" light or dark

" coc-git
" g:coc_git_status including git branch and current project status.
" b:coc_git_status including changed lines of current buffer.
" b:coc_git_blame including blame info of current line.

function! CocCurrentFunction()
  return get(b:, 'coc_current_function', '')
endfunction

" Get remote git repo platform icon
" Gitlab/Github/Gitbuckit/Bare/
" 
" 
" TODO
function! GitRepoPlatform()
  return ''
endfunction

" Git branch
function! LightlineGitStatus()
  " use coc-git
  " return get(g:, 'coc_git_status', '')
  " use fugitive.vim
  if exists('*FugitiveHead')
    let branch = FugitiveHead()
    " NERD font
    return branch !=# '' ? (''.  ' ' .branch. ' '. GitRepoPlatform()) : ''
  endif
  return ''
endfunction

" Git status (add/modified/untracked/deleted)

" Diagnostic info (error/warn)

" Scroll Indicator
function! ScrollIndicator()
  
endfunction

" NERD font
function! LightlineReadonly()
  return &readonly ? '' : ''
endfunction

" Devicon FileType for lightline
function! LightlineFileType()
  return winwidth(0) > 70? (strlen(&filetype) ? &filetype . ' ' . WebDevIconsGetFileTypeSymbol() : 'no ft'): ''
endfunction

" Devicon FileType for lightline
function! LightlineFormat()
  return winwidth(0) > 70? (&fileformat . ' ' . WebDevIconsGetFileFormatSymbol()): ''
endfunction

function! LightlineFileencoding()
  return winwidth(0) > 70 ? (&fenc !=# '' ? &fenc : &enc) : ''
endfunction

" filename + filetype
function! LightlineFileName()
  return winwidth(0) > 70? expand("%:t") . ' ' . WebDevIconsGetFileTypeSymbol(): expand("%:t")
endfunction


let g:lightline = {
      \ 'colorscheme': 'dracula',
      \ }

let g:lightline.component_function = {
      \   'gst': 'LightlineGitStatus',
      \   'grepo': 'LightlineGitRepoPlatform',
      \   'currentfunction': 'CocCurrentFunction',
      \   'readonly': 'LightlineReadonly',
      \   'filetype': 'LightlineFileType',
      \   'fileencoding': 'LightlineFileencoding',
      \   'fileformat': 'LightlineFormat',
      \   'filename': 'LightlineFileName',
      \ }

let g:lightline.active = {
      \   'left': [ [ 'mode', 'paste' ],
      \             [ 'gst','cocstatus', 'currentfunction', 'readonly', 'filename', 'modified', 'charvaluehex' ] ],
      \   'right': [ [ 'lineinfo' ],
      \            [ 'percent' ],
      \            [ 'fileformat','fileencoding' ] ]
      \}

let g:lightline.component = {
      \ 'charvaluehex': '0x%B',
      \ 'mode': '%{lightline#mode()}',
      \ 'absolutepath': '%F',
      \ 'relativepath': '%f',
      \ 'filename': '%t',
      \ 'modified': '%M',
      \ 'bufnum': '%n',
      \ 'paste': '%{&paste?"PASTE":""}',
      \ 'charvalue': '%b',
      \ 'fileencoding': '%{&fenc!=#""?&fenc:&enc}',
      \ 'fileformat': '%{&ff}',
      \ 'filetype': '%{&ft!=#""?&ft:"no ft"}',
      \ 'percent': '%3p%%',
      \ 'percentwin': '%P',
      \ 'spell': '%{&spell?&spelllang:""}',
      \ 'lineinfo': ' %3l:%-2c',
      \ 'line': '%l',
      \ 'column': '%c',
      \ 'close': '%999X X ',
      \ 'winnr': '%{winnr()}' }

let g:lightline.tabline = {
      \ 'left': [ [ 'tabs' ] ],
      \ 'right': [ ] }

let g:lightline.tab = {
      \ 'active': [ 'tabnum', 'filename', 'modified' ],
      \ 'inactive': [ 'tabnum', 'filename', 'modified' ] }

let g:lightline.separator = { 'right': '','left':'' }
let g:lightline.subseparator = { 'left': '/', 'right': '/' }

let g:lightline.mode_map = {
    \ 'n' : 'N',
    \ 'i' : 'I',
    \ 'R' : 'R',
    \ 'v' : 'V',
    \ 'V' : 'VL',
    \ "\<C-v>": 'VBLK',
    \ 'c' : 'CMD',
    \ 's' : 'SLT',
    \ 'S' : 'SL',
    \ "\<C-s>": 'SBLK',
    \ 't': 'TERM',
    \ }
" }}}
