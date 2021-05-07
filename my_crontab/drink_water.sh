#!/usr/bin/env bash
# macos
title=${title:-"Time for water"}
subtitle=${subtitle:-"Need water"}
content=${content:-"よー相棒、Drink water ok?"}
sound=${sound:-"Kyoko"}
# applescript
display_cmd=$(printf 'display notification "%s" with title "%s" subtitle "%s" sound name "%s"' "$content" "$title" "$subtitle" "$sound" ) 
echo $display_cmd
osascript -e "$display_cmd"
# say -v "$sound" "$content"
