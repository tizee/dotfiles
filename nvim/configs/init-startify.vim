let s:default_slogan = [
\ '     __    __      __  __            __                 ',
\ '    /\ \  /\ \    /\ \/\ \          /\ \                ',
\ '    \ `\`\\/ / ___\ \ \_\ \     __  \ \ \___     ___    ',
\ '     `\ `\ / `/ __`\ \  _  \  /`__`\ \ \  _ `\  / __`\  ',
\ '       `\ \ \/\ \L\ \ \ \ \ \/\ \L\.\_\ \ \ \ \/\ \L\ \ ',
\ '         \ \_\ \____/\ \_\ \_\ \__/.\_\\ \_\ \_\ \____/ ',
\ '          \/_/\/___/  \/_/\/_/\/__/\/_/ \/_/\/_/\/___/  ',
\ '',
\ '',
\ ]

let s:colossal_slogan=[
      \'  888888                888   d8b ',
      \'   "88b                888   Y8P ',
      \'    888                888       ',
      \'    888 .d88b.  .d88b. 888888888 ',
      \'     888d8P  Y8bd8P  Y8b888   888 ',
      \'     8888888888888888888888   888 ',
      \'     88PY8b.    Y8b.    Y88b. 888 ',
      \'     888 "Y8888  "Y8888  "Y888888 ',
      \'   .d88P                          ',
      \' .d88P"                           ',
      \'888P"                             ',
\ '',
\ '',
      \]

let s:roman_slogan=[
      \'   oooo                         .    o8o  ',
      \'   `888                       .o8    `"`  ',
      \'    888  .ooooo.   .ooooo.  .o888oo oooo  ',
      \'    888 d88` `88b d88` `88b   888   `888  ',
      \'    888 888ooo888 888ooo888   888    888  ',
      \'    888 888    .o 888    .o   888 .  888  ',
      \'.o. 88P `Y8bod8P` `Y8bod8P`   "888" o888o ',
      \'`Y888P                                    ',
\ '',
\ '',
      \]

" TODO: use figlet to generate hanzi slogan
" TODO: use a morden figlet alternative program for cjk characters or
" complicated scripts.
" TODO steps to generate following slogan
" use a cli font viewer for rendering non-Latin characters
"let g:cjk_slogan=[
"\ '     ⣀⣀⣀⣤⡤  ',
"\ '       ⣿⢀   ',
"\ '     ⠈⠛⣿⠉⠁  ',
"\ '   ⠠⠤⠴⠖⠛⠒⠒⠚⠶',
"\]

let s:logo=[
      \'████████╗██╗███████╗███████╗███████╗',
      \'╚══██╔══╝██║╚══███╔╝██╔════╝██╔════╝',
      \'   ██║   ██║  ███╔╝ █████╗  █████╗  ',
      \'   ██║   ██║ ███╔╝  ██╔══╝  ██╔══╝  ',
      \'   ██║   ██║███████╗███████╗███████╗',
      \'   ╚═╝   ╚═╝╚══════╝╚══════╝╚══════╝',
      \]

function! s:padstr(str,amt)
    return a:str . repeat(' ',a:amt - len(a:str))
endfunction

function s:longest(l) abort
  let max=0
  for line in a:l
    let llen = strlen(line)
    if llen > l:max
      let l:max = llen
    endif
  endfor
  return max
endfunction

" need to escape control sequence for Chinese qutoes
" 杨花榆荚无才思，惟解漫天作雪飞。
" [33m    -- 韩愈[32m《晚春》[m[m
let s:quotes =[
      \"kk-99",
      \"mao-ze-dong-anthology",
      \"mao-ze-dong-chronicle",
      \"learning",
      \"song",
      \"unix"
      \]
let s:quote=system('fortune ' . join(s:quotes, " ") . ' | sed -r "s/.\[[0-9]*m//g"')
let s:quote_lines=split(s:quote,"\n")
let s:max_line=s:longest(s:quote_lines)
let s:quote_lines=map(s:quote_lines,{_,val->s:padstr(val, s:max_line)})
let g:startify_custom_header= startify#center(s:logo) +
      \ startify#pad(startify#center(s:quote_lines))

let g:startify_bookmarks = [ {'c': '~/.vimrc'}, '~/.zshrc' ]
" A list of commands to execute on selection. Leading colons are optional. It
" supports optional custom indices and/or command descriptions.
let g:startify_commands = [
    \ [':PlugConfig', 'PlugConfig'],
    \ ':CocInfo',
    \ {'h': 'h ref'},
    \ ]
" Startify displays lists. Each list consists of a `type` and optionally a `header`
" an custom `indices`.
let g:startify_lists = [
      \ { 'type': 'dir',       'header': ['   MRU '. getcwd()] },
      \ { 'type': 'bookmarks', 'header': ['   Bookmarks']      },
      \ { 'type': 'commands',  'header': ['   Commands']       },
      \ ]

function! s:open_nerdtree()
    if !argc()
      NERDTree
      wincmd w
    endif
endfunction

" Startup with NERDTree opened
autocmd VimEnter * call <SID>open_nerdtree()

" vim:ft=vim
