" File: styledtext.vim
" Description: vim plugin for 𝕤𝕥𝕪𝕝𝕖𝕕𝕥𝕖𝕩𝕥
" Author: tizee
" Email: 33030965+tizee@users.noreply.github.com
" Github: https://github.com/tizee/dotfiles

if exists('loaded_styledtext_vim') || &cp || v:version < 700
  finish
endif
let g:loaded_styledtext_vim = 1

function! s:insert_after_cursor(text)
  let cur_line_num = line('.')
  let cur_col_num = col('.')
  let cur_line = getline('.')
  " new line: text_before_cursor + inserted_text + text_after_cursor
  let new_line = strpart(cur_line, 0, cur_col_num - 1) . a:text . strpart(cur_line, cur_col_num - 1)
  call setline(cur_line_num, new_line)
  " set cursor position
  call cursor(cur_line_num, cur_col_num + strlen(a:text))
endfunction

" replace selected area with given text
function! s:replace_selected(text)
  " [bufnum, linenum, colnum, off]
  let saved_cursor = getpos(".")
  let [line1, col1] = getpos("'<")[1:2]
  let [line2, col2] = getpos("'>")[1:2]
  echo join([line1, col1, ":", line2, col2],"-")
  let saved_cursor[2] = getcurpos()[4] - 1
  let lines = getline(line1, line2)
  let styled_text = split(a:text, "\n")

  " 1-index based
  let cur_line = min([line1,line2])
  let end_line = max([line1,line2])
  let count = 0
  if cur_line == end_line
    let pre_col = col1 - (&selection == 'inclusive' ? 1 : 2) - 1
    let previous = lines[0][:pre_col]
    if pre_col < 0
      let previous = ""
    endif
    let after_col = col2 - (&selection == 'inclusive' ? 1 : 2) + 1
    let after = lines[0][after_col:]
    if after_col > len(lines[0])
      let after = ""
    endif
    if empty(after)
      let after = ""
    endif
    let lines[count] = previous . styled_text[count] . after
    call setline(cur_line, lines[count])
  else
    while count < len(styled_text) - 1
      " before + changed + after
      let lines[count] = styled_text[count]
      call setline(cur_line, lines[count])
      let cur_line = cur_line + 1
      let count = count + 1
    endwhile
  endif
  " cut the last line after its col
  if len(styled_text) > 1
    let lines[-1] = styled_text[-1] . lines[-1][col2 - (&selection == 'inclusive' ? 1 : 2) + 1:]
    call setline(cur_line, lines[-1])
  endif
  call setpos('.', saved_cursor)
endfunction

function s:get_visual_selected()
  " [bufnum, linenum, colnum, off]
  let [line1, col1] = getpos("'<")[1:2]
  let [line2, col2] = getpos("'>")[1:2]
  let lines = getline(line1, line2)
  if line1 == line2
    " cut the last line after its col
    let lines[0] = lines[0][col1 - 1:col2 - (&selection == 'inclusive' ? 1 : 2)]
  else
    " cut the first line before its col
    let lines[0] = lines[0][col1 - 1:]
    " cut the last line after its col
    let lines[-1] = lines[-1][:col2 - (&selection == 'inclusive' ? 1 : 2)]
  endif
  return lines
endfunction

function! s:styledtext_echo(msg, style, type) range
  if executable("styledtext")
    if empty(a:msg)
      let content = join(s:get_visual_selected(),"\n")
      let output = trim(system(["styledtext", "--letter-style", a:style, "--letter-type", a:type, l:content]))
      call s:replace_selected(l:output)
    else
      let output = trim(system(["styledtext", "--letter-style", a:style, "--letter-type", a:type, a:msg]))
      call s:insert_after_cursor(l:output)
    endif
  else
    echoerr "styledtext not found"
  endif
endfunction

command! -range -nargs=? StyledTextMono <line1>,<line2>call <SID>styledtext_echo("<args>", "normal", "monospace")
command! -range -nargs=? StyledTextScript <line1>,<line2>call <SID>styledtext_echo("<args>", "bold", "script")
command! -range -nargs=? StyledTextFraktur <line1>,<line2>call <SID>styledtext_echo("<args>", "bold", "fraktur")
command! -range -nargs=? StyledTextDoubleStruck <line1>,<line2>call <SID>styledtext_echo("<args>", "bold", "doublestruck")
