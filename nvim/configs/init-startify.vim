" TODO: use figlet to generate hanzi slogan
let g:default_slogan = [
\ '     __    __      __  __            __                 ',
\ '    /\ \  /\ \    /\ \/\ \          /\ \                ',
\ '    \ `\`\\/ / ___\ \ \_\ \     __  \ \ \___     ___    ',
\ '     `\ `\ / `/ __`\ \  _  \  /`__`\ \ \  _ `\  / __`\  ',
\ '       `\ \ \/\ \L\ \ \ \ \ \/\ \L\.\_\ \ \ \ \/\ \L\ \ ',
\ '         \ \_\ \____/\ \_\ \_\ \__/.\_\\ \_\ \_\ \____/ ',
\ '          \/_/\/___/  \/_/\/_/\/__/\/_/ \/_/\/_/\/___/  ',
\ ]

let g:colossal_slogan=[
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
      \]

let g:roman_slogan=[
      \'   oooo                         .    o8o  ',
      \'   `888                       .o8    `"`  ',
      \'    888  .ooooo.   .ooooo.  .o888oo oooo  ',
      \'    888 d88` `88b d88` `88b   888   `888  ',
      \'    888 888ooo888 888ooo888   888    888  ',
      \'    888 888    .o 888    .o   888 .  888  ',
      \'.o. 88P `Y8bod8P` `Y8bod8P`   "888" o888o ',
      \'`Y888P                                    ',
      \]

" TODO: use a morden figlet alternative program for cjk characters or
" complicated scripts.
let g:cjk_slogan=[
\ '     ⣀⣀⣀⣤⡤  ',
\ '       ⣿⢀   ',
\ '     ⠈⠛⣿⠉⠁  ',
\ '   ⠠⠤⠴⠖⠛⠒⠒⠚⠶',
\]

let g:startify_custom_header= 'startify#center(g:roman_slogan)'

" Startup with NERDTree and Startify opened
autocmd VimEnter *
          \   if !argc()
          \ |   Startify
          \ |   NERDTree
          \ |   wincmd w
          \ | endif

" vim:ft=vim
