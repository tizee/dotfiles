" NerdTree {{{

" When displaying directory nodes, this setting tells NERDTree to collapse directories that have only one child. Use one of the following lines for this setting
let NERDTreeCascadeSingleChildDir=0
" This setting disables the 'Bookmarks' label 'Press ? for help' text. Use one
" of the following lines for this setting: >
let NERDTreeMinimalUI=1
let NERDTreeShowHidden=1
" Tells the NERDTree whether to display the bookmarks table on startup.
let NERDTreeShowBookmarks=1
" Where the bookmarks are stored.
let NERDTreeBookmarksFile=expand("$HOME/.NERDTreeBookmarks")
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
                \ 'Renamed'   :'->',
                \ 'Unmerged'  :'═',
                \ 'Deleted'   :'Del',
                \ 'Dirty'     :'*',
                \ 'Ignored'   :'Ig',
                \ 'Clean'     :'Cl',
                \ 'Unknown'   :'??',
                \ }
" }}}

" Quick toggle file manager
" nerdtree
nnoremap <silent> <script> <Plug>ChangePWD2CurrentWindow :<c-u>lcd %:h<CR>

nnoremap <silent> tt :NERDTreeToggle<CR>
" nnoremap <leader>r :NERDTreeFind<cr>
nnoremap <silent> <C-n> :<c-u>execute "normal \<Plug>ChangePWD2CurrentWindow"<CR>:<c-u>NERDTreeCWD<CR>
 
" vim:ft=vim sw=2
