#!/usr/bin/env sh
title=${title:-"Time for Tea"}
subtitle=${subtitle:-"Tea!"}
content=${content:-"Take a break"}
sound=${sound:-"Kyoko"}
# applescript
display_cmd=$(printf 'display notification "%s" with title "%s" subtitle "%s" sound name "%s"' "$content" "$title" "$subtitle" "$sound" ) 
# echo $display_cmd
osascript -e "$display_cmd"
afplay ~/Movies/饮茶.mp3
