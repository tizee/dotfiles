#!/usr/bin/env sh
title=${title:-"Time for beer!"}
subtitle=${subtitle:-"Beer!"}
content=${content:-"Take a break"}
sound=${sound:-"Kyoko"}
# applescript
display_cmd=$(printf 'display notification "%s" with title "%s" subtitle "%s" sound name "%s"' "$content" "$title" "$subtitle" "$sound" ) 
# echo $display_cmd
osascript -e "$display_cmd"
afplay ~/Movies/饮啤酒.mp3
