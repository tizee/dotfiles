let g:keyboard_autoresource=1
let g:keyboard_default_layout='qwerty'
if $COLEMAK_KEYBOARD == 1
"let g:keyboard_default_layout='colemak'
else
" let g:keyboard_default_layout='qwerty'
endif 
let g:keyboard_layout_paths = {
      \  'colemak': expand("$HOME/.config/nvim/keyboard/colemak.vim"),
      \  'qwerty': expand("$HOME/.config/nvim/keyboard/qwerty.vim")
      \ }

