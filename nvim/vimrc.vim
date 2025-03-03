" File: vimrc.vim
" Author: tizee
" Email: 33030965+tizee@users.noreply.github.com
" Github: https://github.com/tizee/dotfiles

" see :help auto-setting
" GENERAL SETTINGS {{{

source ~/.config/nvim/minimal.vim

" ========== neovim options ========= {{{
" see diffrences:
" :help vim_diff.txt
" ture color

if has('nvim')
  " For Neovim 0.1.3 and 0.1.4 < https://github.com/neovim/neovim/pull/2198 >
  let $NVIM_TUI_ENABLE_TRUE_COLOR=1
  " set runtimepath^=~/.vim runtimepath+=~/.vim/after
  let g:python3_host_prog="/opt/homebrew/bin/python3"
  set runtimepath^=~/.vim
  set runtimepath+=~/.vim/after
  " set runtimepath=~/.config/nvim
  " set runtimepath+=~/.config/nvim/after
  " let &packpath = &runtimepath
  " Shows the effects of a command incrementally, as you type.
  set inccommand=nosplit
  " source ~/.vimrc
  " echo $TERM
  " echo $COLORTERM => truecolor
  " curl -sSL https://raw.githubusercontent.com/robertknight/konsole/master/tests/color-spaces.pl | perl

  " ft-markdown-plugin
  let g:markdown_recommended_style = 0
  " ft-python-plugin
  let g:python_recommended_style = 1
else
  " vim
  set runtimepath+=~/.config/nvim
  set runtimepath+=~/.config/nvim/after
endif
"}}}

" ========== OS Specific ========== {{{

if !exists("g:os")
  if has("win64") || has("win32") || has("win16")
    let g:os = "Windows"
  else
    let g:os = substitute(system('uname -s'), '\n', '', '')
  endif
endif

if has("gui_running")
  " Spaces after a comma are ignored.  To include a comma in a font name
  " precede it with a backslash.  Setting an option requires an extra
  " backslash before a space and a backslash.  See also
  " |option-backslash|.  For example: >
  "     :set guifont=Screen15,\ 7x13,font\\,with\\,commas
  if g:os == "Darwin"
    set guifont=JetBrainsMono\ Nerd\ Font\ Mono\:h16
    set guifontwide=LXGW\ WenKai\ Mono\:h16
    " neovim
    if has('python3')
      let g:python3_host_prog="/opt/homebrew/bin/python3.11"
    endif
  elseif g:os == "Linux"
    set guifont=JetBrainsMono\ Nerd\ Font\ Mono\ 18
    " neovim
    if has('python3')
      let g:python3_host_prog=system("which python")
    endif
    " https://stackoverflow.com/questions/2600783/how-does-the-vim-write-with-sudo-trick-work
    " Linux only
    cmap w!! w !sudo tee > /dev/null %
  elseif g:os == "Windows"
    set guifont=JetBrainsMono\ Nerd\ Font\ Mono\ 18
  endif
endif

" ========== Windows ========= {{{

if g:os == "Windows"
  behave mswin
  " set shell program, default is cmd
  set shell=C:\WINDOWS\System32\wsl.exe
  " set shell=\"C:\WINDOWS\System32\cmd.exe"\ -f
  set shellpipe=|
  set shellredir=>
  set shellcmdflag=/c
  " chinese menu
  source $VIMRUNTIME/delmenu.vim
  source $VIMRUNTIME/menu.vim
endif

" }}}

" fzf
if g:os == "Darwin"
  " vim compatible
  set rtp+=/opt/homebrew/opt/fzf
endif
" }}}

" }}}

" VIM PLUG {{{
" nvim: ~/.config/nvim/init.vim -> git repo
" vim: ~/.vimrc -> ~/.config/nvim/init.vim
" expand and resolve symbolic links
if has('nvim')
  let g:vim_config_dir=resolve(stdpath('config')) . "/"
else
  let g:vim_config_dir=fnamemodify(resolve(expand('<sfile>:p')),":h") . "/"
endif

function! s:load_packages()
  let l:packages=g:vim_config_dir . "configs/init-packages.vim"
  execute "source " . l:packages
endfunction

" load plugin configs
call s:load_packages()

" vim-plug cheatsheet
function! Cheatsheet_vim_plug()
  echo ':PlugInstall install plugins'
  echo ':PlugUpdate update plugins'
  echo ':PlugDiff review changes from last update'
  echo ':PlugClean autoremove plugins'
endfunction

" vim-plug key-mappings {{{
nnoremap <leader>pi :PlugInstall<cr>
nnoremap <leader>pu :PlugUpgrade<cr>
nnoremap <leader>ps :PlugStatus<cr>
nnoremap <leader>pc :PlugConfig<cr>

" open plugin help docs
function! s:plug_doc()
  let name = matchstr(getline('.'), '^- \zs\S\+\ze:')
  if has_key(g:plugs, name)
    for doc in split(globpath(g:plugs[name].dir, 'doc/*.txt'), '\n')
      execute 'tabe' doc
    endfor
  endif
endfunction

" open the GitHub URL for a plugin or a commit with the default browser
function! s:plug_gx()
  let line = getline('.')
  let sha  = matchstr(line, '^  \X*\zs\x\{7,9}\ze ')
  let name = empty(sha) ? matchstr(line, '^[-x+] \zs[^:]\+\ze:')
	\ : getline(search('^- .*:$', 'bn'))[2:-2]
  let uri  = get(get(g:plugs, name, {}), 'uri', '')
  if uri !~ 'github.com'
    return
  endif
  let repo = matchstr(uri, '[^:/]*/'.name)
  let url  = empty(sha) ? 'https://github.com/'.repo
	\ : printf('https://github.com/%s/commit/%s', repo, sha)
  call netrw#BrowseX(url, 0)
endfunction

augroup VimPlugPlus
  autocmd! FileType vim-plug nmap <buffer> ? <plug>(plug-preview)
  autocmd! FileType vim-plug nnoremap <buffer> <silent> <C-h> :call <SID>plug_doc()<cr>
  autocmd! FileType vim-plug nnoremap <buffer> <silent> <Tab> :call <SID>plug_gx()<cr>
augroup end

function! s:source_helper(name)
  let plugin_prefix=get(g:,'vim_config_dir') . a:name . "/"
  let plugins=readdir(plugin_prefix)
  for plugin_filename in plugins
    let plugin = plugin_prefix . plugin_filename
    if isdirectory(plugin)
      continue
    endif
    execute 'source '. plugin
  endfor
endfunction

" plugin prototype
" set rtp+=~/dev/grepo_vim/coc-packages/coc-flutter
" source each config file under configs directory
call s:source_helper('configs')
" }}}

" }}}

" TMUX {{{
if !empty($TMUX)

endif

" }}}

" Cursor Style {{{
" Nvim does not have special `t_XX` options nor <t_XX> keycodes to configure
" terminal capabilities. Instead Nvim treats the terminal as any other UI,
" e.g. 'guicursor' sets the terminal cursor style if possible.
if has('nvim')
  augroup reset_nvim_cursor
    autocmd!
    autocmd VimEnter,VimResume * set guicursor=n-v-c:block,i-ci-ve:ver25,r-cr:hor20,o:hor50
      \,a:blinkwait700-blinkoff400-blinkon250-Cursor/lCursor
      \,sm:block-blinkwait175-blinkoff150-blinkon175
  augroup END "reset_nvim_cursor
else
  " vim
  " CSI code for cursors
  " CSI Ps SP q
  " Set cursor style (DECSCUSR), VT520.
  "   Ps = 0  -> blinking block.
  "   Ps = 1  -> blinking block (default).
  "   Ps = 2  -> steady block.
  "   Ps = 3  -> blinking underline.
  "   Ps = 4  -> steady underline.
  "   Ps = 5  -> blinking bar (xterm).
  "   Ps = 6  -> steady bar (xterm).
  let &t_SI .= "\e[5 q" " SI = INSERT mode
  let &t_SR .= "\e[3 q" " SR = REPLACE mode
  let &t_EI .= "\e[1 q" " EI = NORMAL mode (ELSE)
  " cursor style in terminal emulator
  let &t_Cs = "\e[4:3m"
  let &t_Ce = "\e[4:0m"
  hi SpellBad guisp=red gui=undercurl guifg=NONE guibg=NONE ctermfg=NONE ctermbg=NONE term=underline cterm=undercurl ctermul=red
  hi SpellCap guisp=yellow gui=undercurl guifg=NONE guibg=NONE ctermfg=NONE ctermbg=NONE term=underline cterm=undercurl ctermul=yellow
  " Initialize cursor shape/color on startup
  augroup reset_cursor_on_enter
    autocmd!
    autocmd VimEnter * normal! :startinsert :stopinsert
    autocmd VimEnter * redraw!
  augroup END
  " If you are a neovim user then you will need to install a terminfo file that tells neovim about this support.
  " see https://wezfurlong.org/wezterm/faq.html#how-do-i-enable-undercurl-curly-underlines
endif
" }}}

" Color Scheme {{{
function! s:load_colorscheme()
  let g:gruvbox_italic=2
  colorscheme gruvbox
  set background=dark
  " highlight Comment cterm=italic gui=italic
endfunction

call s:load_colorscheme()

function! s:switch_background() abort
  if &background ==# 'dark'
    set background=light
  else
    set background=dark
  endif
endfunction

command! -nargs=0 SwitchBg call <SID>switch_background()

" highlights {{{

" https://vi.stackexchange.com/questions/21546/is-it-possible-to-reverse-an-existing-highlighting-group
" swap fg and bg in highlight group
function SwapHiGroup(group)
    let id = synIDtrans(hlID(a:group))
    for mode in ['cterm', 'gui']
        for g in ['fg', 'bg']
            exe 'let '. mode.g. "=  synIDattr(id, '".
                        \ g."#', '". mode. "')"
            exe "let ". mode.g. " = empty(". mode.g. ") ? 'NONE' : ". mode.g
        endfor
    endfor
    exe printf('hi %s ctermfg=%s ctermbg=%s guifg=%s guibg=%s', a:group, ctermbg, ctermfg, guibg, guifg)
endfunction

" signcolumn highlights
highlight! link SignColumn LineNr
call SwapHiGroup('DiffAdd')
call SwapHiGroup('DiffChange')
call SwapHiGroup('DiffDelete')

highlight link GitGutterAdd DiffAdd
highlight link GitGutterChange DiffChange
highlight link GitGutterDelete DiffDelete
highlight link GitGutterChangeDelete DiffChange

" default: links to CursorLineNr
highlight link GitGutterAddLineNr CursorLineNr
" default: links to CursorLineNr
highlight link GitGutterChangeLineNr CursorLineNr
" default: links to CursorLineNr
highlight link GitGutterDeleteLineNr CurosrLineNr
" default: links to GitGutterChangeLineNr
highlight link GitGutterChangeDeleteLineNr GitGutterChangeLineNr

" }}}
" }}}

" AUTOCMD GROUP {{{

  augroup filetype_misc
    autocmd!
    " ShellScript {{{
    autocmd FileType sh setlocal foldmethod=marker
    autocmd FileType zsh setlocal foldmethod=marker
    autocmd BufWritePost *.zsh set filetype=zsh
    autocmd BufWritePost *.sh set filetype=sh
    " }}}
  " git {{{
    autocmd  BufNewFile,BufRead *.gitignore set filetype=conf
    "}}}
    " tmux {{{
    autocmd FileType tmux setlocal foldmethod=marker
    " }}}
  " yaml {{{
    autocmd FileType yaml setlocal foldmethod=indent
  " }}}
  " c/cpp/objective-c {{{
    autocmd BufNewFile,BufRead,BufWritePost *.m set filetype=objc
    autocmd BufNewFile,BufRead,BufWritePost *.mm set filetype=objcpp
    autocmd BufNewFile,BufRead,BufWritePost *.cc set filetype=cpp
    autocmd BufNewFile,BufRead,BufWritePost *.c set filetype=c
    " }}}
  augroup END

" following options may be overwritten by editorconfig-vim
" VimScript  {{{
function! s:VimAbbrev()
iabbr nnp nnoremap
iabbr vnp vnoremap
iabbr cnp cnoremap
iabbr xnp xnoremap
iabbr onp onoremap
iabbr tnp tnoremap
endfunction

  augroup ft_vim
    autocmd!          | " Deletes all auto-commands in the current group
    " force equalizing window
    autocmd VimResized * tabdo wincmd =
  augroup END
  " }}}
  " python {{{
  aug ft_python
    autocmd!          | " Deletes all auto-commands in the current group
    " nvim's ft-python-plugin overwrites this
    autocmd FileType python setlocal expandtab smarttab nosmartindent
    autocmd FileType python setlocal tabstop=4 softtabstop=4 shiftwidth=4 textwidth=80
  aug end
" }}}
  " lua {{{
  aug ft_lua
    autocmd!          | " Deletes all auto-commands in the current group
    autocmd FileType lua setlocal tabstop=2 softtabstop=2 shiftwidth=2 textwidth=120 noexpandtab smarttab nosmartindent
  aug end
" }}}
" golang  {{{
  aug ft_golang
    autocmd!         | " Deletes all auto-commands in the current group
    autocmd FileType go setlocal ts=4 sts=4 sw=4 noexpandtab
    autocmd BufNewFile,BufRead *.go setlocal foldmethod=syntax foldlevel=3
  aug end
  " }}}
  " markdown {{{
  augroup ft_md
    autocmd!         | " Deletes all auto-commands in the current group
    autocmd FileType markdown setlocal tabstop=2 softtabstop=4 expandtab spell spelllang=en_us dictionary=/usr/share/dict/words
  augroup END
  " }}}

" The following autocommand will cause the quickfix window to open after any grep invocation:
autocmd QuickFixCmdPost *grep* cwindow
" }}}

" ABBREVIATION {{{
" gloabal common abbreviations
iabbrev adn and
iabbrev waht what
iabbrev tehn then
iabbrev ture true
iabbrev mian main

function! SetupCommandAbbrs(from, to)
  exec 'cnoreabbrev <expr> '.a:from
	\ .' ((getcmdtype() ==# ":" && getcmdline() ==# "'.a:from.'")'
	\ .'? ("'.a:to.'") : ("'.a:from.'"))'
endfunction

" Use :C to open coc config
call SetupCommandAbbrs('C', 'CocConfig')
" }}}

" DEBUG {{{
" create a temporary buffer for list :messages
command! MsgBuffer execute 'new' | execute 'setlocal buftype=nofile bufhidden=wipe noswapfile' | put =execute('messages')
" }}}

" vim: set foldmethod=marker foldlevel=0 nomodeline:
