" Use terminals that support oscyank
if exists(":OSCYank") == 2
  vnoremap <C-c> y:OSCYankVisual<cr>
else
  vnoremap <C-c> y:OSCYankVisual<cr>
endif
