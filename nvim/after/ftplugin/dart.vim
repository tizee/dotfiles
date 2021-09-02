" flutter run 
nmap <silent> <leader>o1 :<C-u>CocCommand flutter.run<CR>

" similar to flutter's keybindings in CLI
" hot restart
nmap <silent> <leader>or :<C-u>CocCommand flutter.dev.hotRestart<CR>
" toggle the display of construction lines
nmap <silent> <leader>op :<C-u>CocCommand flutter.dev.debugPaintSizeEnabled<CR>
" toggle widget inspector
nmap <silent> <leader>og :<C-u>CocCommand flutter.dev.showWidgetInspectorOverride<CR>
