" see https://github.com/junegunn/fzf/blob/master/README-VIM.md
"
" fzf#run([spec dict])
" Starts fzf inside Vim with the given spec
" :call fzf#run({'source': 'ls'})
" fzf#wrap([spec dict]) -> (dict)
" Takes a spec for fzf#run and returns an extended version of it with additional options for addressing global preferences (g:fzf_xxx)
" :echo fzf#wrap({'source': 'ls'})
" We usually wrap a spec with fzf#wrap before passing it to fzf#run
" :call fzf#run(fzf#wrap({'source': 'ls'}))
" :FZF [fzf_options string] [path string]
" Basic fuzzy file selector
" A reference implementation for those who don't want to write VimScript to implement custom commands
" If you're looking for more such commands, check out fzf.vim project.
"
" g:fzf_action
" Customizable extra key bindings for opening selected files in different ways
" g:fzf_layout
" Determines the size and position of fzf window
" g:fzf_colors
" Customizes fzf colors to match the current color scheme
" g:fzf_history_dir
" Enables history feature
"
if exists('loaded_tz_fzf_vim') || &cp || v:version < 700
  finish
endif
let g:loaded_tz_fzf_vim = 1
let s:is_win = has('win32') || has('win64')
let s:bin_dir = expand('<sfile>:p:h').'/fzf-scripts/'
let s:bin = {
\ 'preview': s:bin_dir.'preview.sh',
\ 'tags':    s:bin_dir.'tags.pl' }
let s:TYPE = {'dict': type({}), 'funcref': type(function('call')), 'string': type(''), 'list': type([])}
if s:is_win
  if has('nvim')
    let s:bin.preview = split(system('for %A in ("'.s:bin.preview.'") do @echo %~sA'), "\n")[0]
  else
    let s:bin.preview = fnamemodify(s:bin.preview, ':8')
  endif
endif

" fzf general options {{{
" nnoremap <C-c> <nop>
let $FZF_DEFAULT_OPTS .= ' --inline-info --reverse' " --bind ctrl-c:select-all
" Enable per-command history
" - History files will be stored in the specified directory
" - When set, CTRL-N and CTRL-P will be bound to 'next-history' and
"   'previous-history' instead of 'down' and 'up'.
let g:fzf_history_dir = '~/.local/share/fzf-history'

" send results to quickfix list from fzf result 
" https://github.com/junegunn/fzf.vim/issues/185

function! s:make_quickfix_list(lines)
  call setqflist(map(copy(a:lines),'{ "filename": v:val }' ))
  copen
  cc
endfunction

let g:fzf_action = {
      \ 'ctrl-q': function('s:make_quickfix_list'),
      \ 'ctrl-e': 'edit',
      \ 'ctrl-t': 'tab split',
      \ 'ctrl-x': 'split',
      \ 'ctrl-v': 'vsplit' }

let s:default_action = g:fzf_action

" git commit
let g:fzf_commits_log_options = '--graph --color=always --format="%C(auto)%h%d %s %C(black)%C(bold)%cr"'

let g:fzf_colors =
\ { 'fg':      ['fg', 'Normal'],
  \ 'bg':      ['bg', 'Normal'],
  \ 'hl':      ['fg', 'Comment'],
  \ 'fg+':     ['fg', 'CursorLine', 'CursorColumn', 'Normal'],
  \ 'bg+':     ['bg', 'CursorLine', 'CursorColumn'],
  \ 'hl+':     ['fg', 'Statement'],
  \ 'info':    ['fg', 'PreProc'],
  \ 'border':  ['fg', 'Ignore'],
  \ 'prompt':  ['fg', 'Conditional'],
  \ 'pointer': ['fg', 'Exception'],
  \ 'marker':  ['fg', 'Keyword'],
  \ 'spinner': ['fg', 'Label'],
  \ 'header':  ['fg', 'Comment'] }

" Terminal buffer options for fzf
augroup TZFZF
autocmd! FileType fzf
autocmd! FileType fzf set noshowmode noruler nonu nornu
augroup END "TZFZF

" let g:fzf_preview_window = 'right:60%'
if exists('$TMUX')
  let g:fzf_layout = { 'tmux': '-p90%,60%' }
endif

if has('nvim') && exists('&winblend') && &termguicolors
  set winblend=20

  hi NormalFloat guibg=None
  if exists('g:fzf_colors.bg')
     call remove(g:fzf_colors, 'bg')
  endif

  if stridx($FZF_DEFAULT_OPTS, '--border') == -1
    let $FZF_DEFAULT_OPTS .= ' --border'
  endif

  function! FloatingFZF()
    let width = float2nr(&columns * 0.8)
    let height = float2nr(&lines * 0.6)
    let opts = { 'relative': 'editor',
               \ 'row': (&lines - height) / 2,
               \ 'col': (&columns - width) / 2,
               \ 'width': width,
               \ 'height': height }

    call nvim_open_win(nvim_create_buf(v:false, v:true), v:true, opts)
  endfunction

  let g:fzf_layout = { 'window': 'call FloatingFZF()' }
endif
" }}}

" fzf enhanced commands {{{

" utilities {{{
function! s:extend_opts(dict, eopts, prepend)
  if empty(a:eopts)
    return
  endif
  if has_key(a:dict, 'options')
    if type(a:dict.options) == s:TYPE.list && type(a:eopts) == s:TYPE.list
      if a:prepend
        let a:dict.options = extend(copy(a:eopts), a:dict.options)
      else
        call extend(a:dict.options, a:eopts)
      endif
    else
      let all_opts = a:prepend ? [a:eopts, a:dict.options] : [a:dict.options, a:eopts]
      let a:dict.options = join(map(all_opts, 'type(v:val) == s:TYPE.list ? join(map(copy(v:val), "fzf#shellescape(v:val)")) : v:val'))
    endif
  else
    let a:dict.options = a:eopts
  endif
endfunction

function! s:merge_opts(dict, eopts)
  return s:extend_opts(a:dict, a:eopts, 0)
endfunction

function! s:prepend_opts(dict, eopts)
  return s:extend_opts(a:dict, a:eopts, 1)
endfunction

function! s:wrap(name, opts, bang)
  " fzf#wrap does not append --expect if sink or sink* is found
  let opts = copy(a:opts)
  let options = ''
  if has_key(opts, 'options')
    let options = type(opts.options) == s:TYPE.list ? join(opts.options) : opts.options
  endif
  if options !~ '--expect' && has_key(opts, 'sink*')
    let Sink = remove(opts, 'sink*')
    let wrapped = fzf#wrap(a:name, opts, a:bang)
    let wrapped['sink*'] = Sink
  else
    let wrapped = fzf#wrap(a:name, opts, a:bang)
  endif
  return wrapped
endfunction

function! s:fzf(name, opts, extra)
  let [extra, bang] = [{}, 0]
  if len(a:extra) <= 1
    let first = get(a:extra, 0, 0)
    if type(first) == s:TYPE.dict
      let extra = first
    else
      let bang = first
    endif
  elseif len(a:extra) == 2
    let [extra, bang] = a:extra
  else
    throw 'invalid number of arguments'
  endif

  let eopts  = has_key(extra, 'options') ? remove(extra, 'options') : ''
  let merged = extend(copy(a:opts), extra)
  call s:merge_opts(merged, eopts)
  return fzf#run(s:wrap(a:name, merged, bang))
endfunction

function! s:action_for(key, ...)
  let default = a:0 ? a:1 : ''
  let Cmd = get(get(g:, 'fzf_action', s:default_action), a:key, default)
  return type(Cmd) == s:TYPE.string ? Cmd : default
endfunction

function! s:fill_quickfix(list, ...)
  if len(a:list) > 1
    call setqflist(a:list)
    copen
    wincmd p
    if a:0
      execute a:1
    endif
  endif
endfunction

function! s:ag_to_qf(line, has_column)
  let parts = matchlist(a:line, '\(.\{-}\)\s*:\s*\(\d\+\)\%(\s*:\s*\(\d\+\)\)\?\%(\s*:\(.*\)\)\?')
  let dict = {'filename': &acd ? fnamemodify(parts[1], ':p') : parts[1], 'lnum': parts[2], 'text': parts[4]}
  if a:has_column
    let dict.col = parts[3]
  endif
  return dict
endfunction

function! s:escape(path)
  let path = fnameescape(a:path)
  return s:is_win ? escape(path, '$') : path
endfunction

function! s:open(cmd, target)
  if stridx('edit', a:cmd) == 0 && fnamemodify(a:target, ':p') ==# expand('%:p')
    return
  endif
  execute a:cmd s:escape(a:target)
endfunction

function! s:ag_handler(lines, has_column)
  if len(a:lines) < 2
    return
  endif

  let cmd = s:action_for(a:lines[0], 'e')
  let list = map(filter(a:lines[1:], 'len(v:val)'), 's:ag_to_qf(v:val, a:has_column)')
  if empty(list)
    return
  endif

  let first = list[0]
  try
    call s:open(cmd, first.filename)
    execute first.lnum
    if a:has_column
      execute 'normal!' first.col.'|'
    endif
    normal! zz
  catch
  endtry

  call s:fill_quickfix(list)
endfunction

function! s:fzf_grep(grep_command, has_column, ...)
  let words = []
  for word in split(a:grep_command)
    if word !~# '^[a-z]'
      break
    endif
    call add(words, word)
  endfor
  let words   = empty(words) ? ['grep'] : words
  let name    = join(words, '-')
  let capname = join(map(words, 'toupper(v:val[0]).v:val[1:]'), '')
  let opts = {
  \ 'column':  a:has_column,
  \ 'options': ['--ansi', '--prompt', capname.'> ',
  \             '--multi', '--bind', 'alt-a:select-all,alt-d:deselect-all',
  \             '--delimiter', ':', '--preview-window', '+{2}-5',
  \             '--color', 'hl:4,hl+:12']
  \}
  function! opts.sink(lines)
    return s:ag_handler(a:lines, self.column)
  endfunction
  let opts['sink*'] = remove(opts, 'sink')
  try
    let prev_default_command = $FZF_DEFAULT_COMMAND
    let $FZF_DEFAULT_COMMAND = a:grep_command
    return s:fzf(name, opts, a:000)
  finally
    let $FZF_DEFAULT_COMMAND = prev_default_command
  endtry
endfunction

function! s:fzf_with_preview(...)
  " Default spec
  let spec = {}
  let window = ''

  let args = copy(a:000)

  " Spec to wrap
  if len(args) && type(args[0]) == s:TYPE.dict
    let spec = copy(args[0])
    call remove(args, 0)
  endif

  if !executable('bash')
    if !s:warned
      call s:warn('Preview window not supported (bash not found in PATH)')
      let s:warned = 1
    endif
    return spec
  endif

  " Placeholder expression (TODO/TBD: undocumented)
  let placeholder = get(spec, 'placeholder', '{}')

  " Preview window
  if len(args) && type(args[0]) == s:TYPE.string
    if args[0] !~# '^\(up\|down\|left\|right\)'
      throw 'invalid preview window: '.args[0]
    endif
    let window = args[0]
    call remove(args, 0)
  endif

  let preview = []
  if len(window)
    let preview += ['--preview-window', window]
  endif
  if s:is_win
    let is_wsl_bash = exepath('bash') =~? 'Windows[/\\]system32[/\\]bash.exe$'
    let preview_cmd = 'bash '.(is_wsl_bash
    \ ? substitute(substitute(s:bin.preview, '^\([A-Z]\):', '/mnt/\L\1', ''), '\', '/', 'g')
    \ : escape(s:bin.preview, '\'))
  else
    let preview_cmd = fzf#shellescape(s:bin.preview)
  endif
  let preview += ['--preview', preview_cmd.' '.placeholder]

  if len(args)
    call extend(preview, ['--bind', join(map(args, 'v:val.":toggle-preview"'), ',')])
  endif
  call s:merge_opts(spec, preview)
  return spec
endfunction
" }}}

" All files with fd {{{
command! -nargs=? -complete=dir AF
  \ call fzf#run(fzf#wrap(s:fzf_with_preview({
  \   'source': 'fd --type f --hidden --follow --exclude .git --no-ignore ' . <q-args>
  \ })))
" }}}

" file search with ag {{{
command! -bang -nargs=* AG
      \ call fzf#vim#ag(<q-args>,
      \                 <bang>0 ? s:fzf_with_preview('up:60%')
      \                         : s:fzf_with_preview('right:50%:hidden', '?'),
      \                 <bang>0)
" }}}

" content search with ripgrep {{{
function! s:ripgrepFzf(query, fullscreen)
  let command_fmt = 'rg --column --line-number --no-heading --color=always --smart-case -- %s'
  let initial_command = printf(command_fmt, shellescape(a:query))
  let reload_command = printf(command_fmt, '{q} || true')
  let spec = {'options': ['--phony', '--query', a:query, '--bind', 'change:reload:'.reload_command]}
  call s:fzf_grep(initial_command, 1, s:fzf_with_preview(spec), a:fullscreen)
endfunction

command! -nargs=* -bang RG call <SID>ripgrepFzf(<q-args>, <bang>0)

function! s:rgWordFzf(query, fullscreen)
  " get word
  let word = execute('normal viwy')
  if len(word)
    let command_fmt = 'rg --column --line-number --no-heading --color=always --smart-case -- %s || true'
    let initial_fmt= 'rg --column --line-number --no-heading --color=always --smart-case ' . word . ' || true'
    let reload_command = printf(command_fmt, '{q}')
    let spec = {'options': ['--phony', '--query', a:query, '--bind', 'change:reload:'.reload_command]}
    call s:fzf_grep(initial_command, 1, s:fzf_with_preview(spec), a:fullscreen)
  endif
  " throw error
endfunction

command! -nargs=* -bang RgWordFzf call <SID>rgWordFzf(<q-args>, <bang>0)
" }}}


" git grep {{{
command! -bang -nargs=* GGrep
  \ call s:fzf_grep(
  \   'git grep --line-number -- '.shellescape(<q-args>), 0,
  \   s:fzf_with_preview({'dir': systemlist('git rev-parse --show-toplevel')[0]}), <bang>0)
" }}}

" plug help {{{
if exists('g:plugs')
  function! s:plug_help_sink(line)
    let dir = g:plugs[a:line].dir
    for path in ['doc/*.text','README.md']
      let match = get(split(globpath(dir,path),"\n"),0,'')
      if len(match)
        " show in new tab
        execute 'tabedit' match
        return
      endif
    endfor
    tabnew
    execute 'Explore' dir
  endfunction

  command! PlugHelp call fzf#run(fzf#wrap({
    \ 'source': sort(keys(g:plugs)),
    \ 'sink': function('s:plug_help_sink')}))
endif 
" }}}

" Quick edit oldfiles {{{
function! s:open_file(file) abort
  let absolute_path = resolve(a:file)
  if empty(absolute_path) || !filereadable(absolute_path)
    echohl Error
    echom "Cannnot read " . absolute_path
    echohl Normal
    return
  endif
  " show in new tab
  execute 'tabedit' absolute_path
endfunction

" v:oldfiles depends on 'shada' option
function! s:filter_oldfiles()
  let oldfiles = []
  for file in v:oldfiles
    if !isdirectory(file) && filereadable(file)
      call add(oldfiles, file)
    endif
  endfor
  return oldfiles
endfunction

command! -nargs=* Ze call fzf#run(fzf#wrap({
      \ 'source': s:filter_oldfiles(),
      \ 'sink': function('s:open_file')}))
" }}}

" list files {{{
" The query history for this command will be stored as 'ls' inside g:fzf_history_dir.
" The name is ignored if g:fzf_history_dir is not defined.
command! -bang -complete=dir -nargs=* LS
    \ call fzf#run(fzf#wrap('ls', {'source': 'ls', 'dir': <q-args>}, <bang>0))
" }}}

" buffer close {{{
function! s:list_buffers()
  redir => bf_list
  silent ls
  redir END
  return split(bf_list,"\n")
endfunction

function! s:close_buffers(lines)
  execute 'bwipeout' join(map(a:lines,{_,line -> split(line)[0]}))
endfunction

" close selected buffer
command! BD call fzf#run(fzf#wrap({
\ 'source': s:list_buffers(),
\ 'sink*': {lines -> s:close_buffers(lines)},
\ 'options': '--multi --reverse'
\ }))
" }}}

" Plugin Config Helpers {{{

" list config files
function! s:list_my_configs()
  let res=[]
  let files = split(globpath('~/.config/nvim/configs', '*.vim'), '\n')
  for file in files
    let match=matchstr(get(split(file,'configs/'),1,''),'\v(.*)\ze\.vim')
    if len(match)
      call add(res,match)
    endif
  endfor
  return res
endfunction

" open custom plugin config file
function! s:open_my_config_sink(line)
  if has('nvim')
    let dir = resolve(stdpath('config')) . '/configs'
  else
    let dir = resolve('~/.config/nvim/configs')
  endif 
  let match = get(split(globpath(dir,a:line . '.vim'),"\n"),0,'')
  if len(match)
    " show in new tab
    execute 'tabedit' match
    return
  endif
  tabnew
  execute 'Explore' match
endfunction

command! PlugConfig call fzf#run(fzf#wrap({
  \ 'source': s:list_my_configs(),
  \ 'sink': {lines -> s:open_my_config_sink(lines)}
  \ }))

" }}}

" }}}

" fzf mappings{{{
nnoremap <leader>fb :BD<CR>
nnoremap <leader>fg :GGrep<CR>
nnoremap <leader>fa :AG<CR>
nnoremap <leader>fr :RG<CR>
nnoremap <leader>fh :PlugHelp<CR>
" }}}

