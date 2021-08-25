" NerdTree {{{

" Can be enabled or disabled
let g:webdevicons_enable_nerdtree = 1
" whether or not to show the nerdtree brackets around flags
let g:webdevicons_conceal_nerdtree_brackets = 0

let g:NERDTreeGitStatusUseNerdFonts = 1 " you should install nerdfonts by yourself. default: 0
let g:NERDTreeIgnore = ['^node_modules$']
let g:NERDTreeGitStatusIndicatorMapCustom = {
                \ 'Modified'  :'!',
                \ 'Staged'    :'+',
                \ 'Untracked' :'?',
                \ 'Renamed'   :'➜',
                \ 'Unmerged'  :'═',
                \ 'Deleted'   :'[D]',
                \ 'Dirty'     :'[Dirty]',
                \ 'Ignored'   :'*',
                \ 'Clean'     :'[C]',
                \ 'Unknown'   :'??',
                \ }
" }}}

" vim:ft=vim sw=2
