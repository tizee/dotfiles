" VIM-AUTOFORMAT {{{
let g:formatterpath = ['/usr/local/bin/astyle','/Users/tizee/.cargo/bin/rustfmt', '/usr/local/go/bin/gofmt']
let b:formatdef_custom_c='"astyle --mode=c --style=kr"'
let b:formatters_c = ['custom_c']

" }}}
