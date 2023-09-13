#!/usr/bin/env zsh

# reference from https://til.simonwillison.net/macos/downloading-partial-youtube-videos
# arguments: $1: url, $2: time stamp -5 seconds, $3: length of clip (seconds), $4: video file name
function __clip_youtube_vidoe() {
	if command -v yt-dlp > /dev/null 2>&1 ; then
		if [[ $# != 4 ]]; then
			printf "usage: 1: url, 2: time stamp -5 seconds HH:MM:SS, 3: length of clip (seconds), 4: video file name\n"
		else
			# zsh array index from 1
			urls=("${(@f)$(yt-dlp --youtube-skip-dash-manifest -g "$1")}")
			len=${#urls[@]}
			echo "$len"
			if [[ len -gt 1 ]]; then
				echo "0:${urls[1]} \n1:${urls[2]}"
				ffmpeg -http_proxy http://127.0.0.1:7890 -ss $2 -i "${urls[1]}" -ss $2 -i "${urls[2]}" -ss 5 -map 0:v -map 1:a -c:v libx264 -c:a aac -t $3 $4
			else
				ffmpeg -http_proxy http://127.0.0.1:7890 -ss $2 -i "${urls[1]}" -ss 5 -c:v libx264 -c:a aac -t $3 $4
			fi
		fi
	fi
}

__clip_youtube_vidoe "$@"
