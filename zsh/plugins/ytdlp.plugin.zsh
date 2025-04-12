#!/usr/bin/env zsh

function ytdl() {
    # YouTube 1080P 视频下载函数
    # 用法: ytdl [视频URL] [选项]

    # 检查参数
    if [[ $# -lt 1 ]]; then
        echo "Usage: ytdl <video URL> [options]"
        echo "Options:"
        echo "  --playlist          Download playlist"
        echo "  --browser <browser> Use cookies from browser(chrome, firefox, edge, safari, brave...)"
        return 1
    fi

    # 设置变量
    local VideoUrl=$1
    shift

    # 默认设置
    local use_playlist=false
    local use_browser_cookies=false
    local browser=""

    # 解析选项
    while [[ $# -gt 0 ]]; do
        case $1 in
            --playlist)
                use_playlist=true
                shift
                ;;
            --browser)
                use_browser_cookies=true
                browser=$2
                shift 2
                ;;
            *)
                echo "Unknown option: $1"
                return 1
                ;;
        esac
    done

    # 设置基本命令
    local cmd=(yt-dlp --no-mtime)

    # 添加选项
    $use_playlist && cmd+=(--yes-playlist) || cmd+=(--no-playlist)
    cmd+=(--audio-format best --format 'bestvideo[height=1080]+bestaudio/best[height<=1080]/best')
    cmd+=(--merge-output-format mp4)
    $use_browser_cookies && cmd+=(--cookies-from-browser $browser)

    # 添加URL
    cmd+=($VideoUrl)

    # 执行命令
    "${cmd[@]}"

    echo "Download completed!"
}

