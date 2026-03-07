#!/usr/bin/env zsh

function ytdl() {
    # YouTube 1080P 视频下载函数
    # 用法: ytdl [视频URL] [选项]

    # 检查参数
    if [[ $# -lt 1 ]]; then
        echo "Usage: ytdl <video URL> [options]"
        echo "Options:"
        echo "  --playlist          Download playlist"
        echo "  --browser <browser> Use cookies from browser (default: firefox, chrome, edge, safari, brave...)"
        return 1
    fi

    # 设置变量
    local VideoUrl=$1
    shift

    # 默认设置
    local use_playlist=false
    local use_browser_cookies=true
    local browser="firefox"

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

    # 执行命令，失败时自动fallback
    local exit_code=0
    "${cmd[@]}" || exit_code=$?

    # 如果失败(403等)，尝试使用android客户端作为fallback
    if [[ $exit_code -ne 0 ]]; then
        echo "Primary format failed, trying fallback with android client..."

        local fallback_cmd=(yt-dlp --no-mtime --extractor-args "youtube:player_client=android")
        $use_playlist && fallback_cmd+=(--yes-playlist) || fallback_cmd+=(--no-playlist)
        fallback_cmd+=(--format 'best[ext=mp4]/best')
        fallback_cmd+=(--merge-output-format mp4)
        $use_browser_cookies && fallback_cmd+=(--cookies-from-browser $browser)
        fallback_cmd+=($VideoUrl)

        "${fallback_cmd[@]}" || exit_code=$?
    fi

    if [[ $exit_code -eq 0 ]]; then
        echo "Download completed!"
    else
        echo "Download failed!"
    fi

    return $exit_code
}

