let g:slogan = [
\ '     __    __      __  __            __                 ',
\ '    /\ \  /\ \    /\ \/\ \          /\ \                ',
\ '    \ `\`\\/ / ___\ \ \_\ \     __  \ \ \___     ___    ',
\ '     `\ `\ / `/ __`\ \  _  \  /`__`\ \ \  _ `\  / __`\  ',
\ '       `\ \ \/\ \L\ \ \ \ \ \/\ \L\.\_\ \ \ \ \/\ \L\ \ ',
\ '         \ \_\ \____/\ \_\ \_\ \__/.\_\\ \_\ \_\ \____/ ',
\ '          \/_/\/___/  \/_/\/_/\/__/\/_/ \/_/\/_/\/___/  ',
\ ]
let g:startify_custom_header= 'startify#center(g:slogan)'

" Startup with NERDTree and Startify opened
autocmd VimEnter *
          \   if !argc()
          \ |   Startify
          \ |   NERDTree
          \ |   wincmd w
          \ | endif

" vim:ft=vim
