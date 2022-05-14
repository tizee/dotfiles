if exists('loaded_my_json_vim') || &cp || v:version < 700
  finish
endif
let g:loaded_my_json_vim = 1

command! -nargs=0 DisableJsonDisplay set conceallevel=0
