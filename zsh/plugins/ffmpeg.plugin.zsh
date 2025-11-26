#!/usr/bin/env zsh

# Guard: prevent reloading
(( ${+_FFMPEG_PLUGIN_LOADED} )) && return
typeset -g _FFMPEG_PLUGIN_LOADED=1

get_simple_audio() {
    # Check if required arguments are provided
  if [ $# -lt 2 ]; then
    echo "Usage: $0 <input_file> <output_name>"
    exit 1
  fi

  # Check if input file exists
  if [ ! -f "$1" ]; then
    echo "Error: Input file '$1' not found"
    exit 1
  fi

  # Create output directory if it doesn't exist
  output_dir=$(dirname "$2")
  if [ "$output_dir" != "." ] && [ ! -d "$output_dir" ]; then
    mkdir -p "$output_dir"
  fi

  # Use ffmpeg with error handling
  ffmpeg -v error \
    -i "$1" \
    -ar 16000 \
    -ac 1 \
    -map 0:a \
    -c:a mp3 \
    "${2}.mp3" || { echo "Error: ffmpeg conversion failed"; exit 1; }

  echo "Conversion complete: ${2}.mp3"
}

# Function to extract audio from video
extract_video_audio() {
    if [[ $# -lt 2 ]]; then
        echo "Usage: extract_video_audio input_file output_file"
        echo "  input_file: Path to the input video file"
        echo "  output_file: Path to the output audio file (without extension)"
        return 1
    fi

    local file="$1"
    local output="$2"

    # Check if file exists
    if [[ ! -f "$file" ]]; then
        echo "Error: Input file '$file' does not exist"
        return 1
    fi

    ffmpeg -i "$file" -c:a aac -q:a 2 "${output}.m4a"
}

# Function to get ffmpeg encoders in JSON format
ffmpeg_encoders_json() {
    # 捕获 ffmpeg 输出，同时将标准输入指向 /dev/null，防止 ffmpeg 误判交互式终端
    local ffmpeg_output
    ffmpeg_output=$(ffmpeg -hide_banner -encoders </dev/null 2>&1)

    # 初始化编码器数组
    local video_encoders=()
    local audio_encoders=()
    local subtitle_encoders=()

    local found_separator=false

    # 使用 split 函数（zsh 特有）按行分割输出
    local -a lines=("${(@f)ffmpeg_output}")

    for line in "${lines[@]}"; do
        # 使用 zsh 参数扩展进行修剪，避免使用 echo 和 xargs
        local trimmed_line="${line##[[:space:]]}"
        trimmed_line="${trimmed_line%%[[:space:]]}"

        if [[ "$found_separator" == "false" ]]; then
            if [[ "$trimmed_line" =~ ^------ ]]; then
                found_separator=true
            fi
            continue
        fi

        # 使用 zsh 的数组分割功能
        local -a fields=("${(@s: :)trimmed_line}")
        [[ ${#fields[@]} -lt 2 ]] && continue

        local code="${fields[1]}"
        local encoder="${fields[2]}"

        # 根据 code 首字符判断编码器类型
        case "${code:0:1}" in
            V) video_encoders+=("$encoder") ;;
            A) audio_encoders+=("$encoder") ;;
            S) subtitle_encoders+=("$encoder") ;;
        esac
    done

    # 生成 JSON 输出，仅输出 JSON 内容
    jq -n \
        --argjson video_encoder "$(printf '%s\n' "${video_encoders[@]}" | jq -R . | jq -s .)" \
        --argjson audio_encoder "$(printf '%s\n' "${audio_encoders[@]}" | jq -R . | jq -s .)" \
        --argjson subtitle_encoder "$(printf '%s\n' "${subtitle_encoders[@]}" | jq -R . | jq -s .)" \
        '{video_encoder: $video_encoder, audio_encoder: $audio_encoder, subtitle_encoder: $subtitle_encoder}'
}

# Function to get video encoder list
get_video_encoder_list() {
    ffmpeg_encoders_json | jq '.video_encoder'
}

# Function to get audio encoder list
get_audio_encoder_list() {
    ffmpeg_encoders_json | jq '.audio_encoder'
}

# Function to search for encoders
ffmpeg_search_encoder() {
    if [[ $# -lt 1 ]]; then
        echo "Usage: ffmpeg_search_encoder encoder_name [--audio]"
        echo "  encoder_name: Name or partial name of the encoder to search for"
        echo "  --audio: Search in audio encoders instead of video encoders"
        return 1
    fi

    local encoder="$1"
    local audio=false

    if [[ "$2" == "--audio" ]]; then
        audio=true
    fi

    if [[ "$audio" == "true" ]]; then
        get_audio_encoder_list | jq -r ".[] | select(contains(\"${encoder}\"))"
    else
        get_video_encoder_list | jq -r ".[] | select(contains(\"${encoder}\"))"
    fi
}

# Function to get video information
get_video_info() {
    if [[ $# -lt 1 ]]; then
        echo "Usage: get_video_info input_file"
        echo "  input_file: Path to the input video file"
        return 1
    fi

    local file="$1"

    # Check if file exists
    if [[ ! -f "$file" ]]; then
        echo "Error: Input file '$file' does not exist"
        return 1
    fi

    ffprobe -v error -select_streams v:0 -show_entries stream=codec_name,codec_long_name,profile,codec_type,codec_tag_string,codec_tag,width,height,coded_width,coded_height,display_aspect_ratio -of json "$file"
}

# Function to get video codec information
get_video_codec() {
    if [[ $# -lt 1 ]]; then
        echo "Usage: get_video_codec input_file"
        echo "  input_file: Path to the input video file"
        return 1
    fi

    local file="$1"
    get_video_info "$file" | jq '.streams[] | {codec_name, codec_long_name, codec_tag_string}'
}

# Function to convert video with (optional) hardware acceleration
# Now using CRF (or -q:v) instead of manually setting bitrate
convert_video() {
    if [[ $# -lt 2 ]]; then
        echo "Usage: convert_video input_file output_file [format] [quality] [crf]"
        echo "  input_file : Path to the input video file"
        echo "  output_file: Path to the output video file"
        echo "  format     : h264 or h265 (default: h264)"
        echo "  quality    : ultrafast, superfast, veryfast, faster, fast, medium, slow, slower, veryslow (default: medium)"
        echo "  crf        : CRF value (0~51, default: 18)."
        echo "               - For hardware mode, we will use '-q:v' to approximate."
        return 1
    fi

    local file="$1"
    local output="$2"
    local format="${3:-h264}"
    local preset="${4:-veryslow}"
    local crf="${5:-18}"  # 默认 CRF=18

    # Check if file exists
    if [[ ! -f "$file" ]]; then
        echo "Error: Input file '$file' does not exist"
        return 1
    fi

    echo "Processing file: $file"
    echo "Output file: $output"

    # Check if output file already exists
    if [[ -f "$output" ]]; then
        echo -n "Output file '$output' already exists. Overwrite? (Y/N): "
        read -r overwrite
        if [[ "$overwrite" != "Y" && "$overwrite" != "y" ]]; then
            echo "Operation cancelled. File exists and overwrite denied."
            return 1
        fi
    fi

    # Choose codec based on format
    local codec=""
    if [[ "$format" == "h265" ]]; then
        codec="hevc"
    else
        codec="h264"
    fi

    # Check if VideoToolbox is available for the chosen codec
    local hw_encoder="${codec}_videotoolbox"
    local hw_available=false
    if ffmpeg -hide_banner -encoders 2>&1 | grep -q "$hw_encoder"; then
        hw_available=true
    fi

    # 如果是macOS硬件加速，VideoToolbox并没有libx264那样的CRF模式，
    # 这里用 -q:v 来模拟一个相对恒定质量，但实际效果仍不同于真正CRF。
    if [[ "$hw_available" == "true" ]]; then
        echo "Attempting hardware acceleration with $hw_encoder..."
        # Hardware encoding with approximate "constant quality"
        # Note: -q:v 的数值范围跟CRF并不一致，需要自行调整测试。
        # 常见可尝试范围：10~35（数值越低画质越好体积越大）
        if ffmpeg -i "$file" \
                   -c:v "$hw_encoder" \
                   -q:v "$crf" \
                   -preset "$preset" \
                   -c:a copy \
                   "$output"
        then
            echo "Hardware encoding successful."
            return 0
        else
            echo "Hardware encoding failed. Falling back to software encoding."
        fi
    else
        echo "Hardware encoder not found or not available for '$codec'. Using software encoding."
    fi

    # ---------------------------
    # Software encoding with real CRF
    # ---------------------------
    local sw_codec="libx264"
    [[ "$format" == "h265" ]] && sw_codec="libx265"

    echo "Using software encoding: $sw_codec with CRF=$crf, preset=$preset"
    ffmpeg -i "$file" \
           -c:v "$sw_codec" \
           -preset "$preset" \
           -crf "$crf" \
           -c:a copy \
           "$output"
}

# Function to compress video
compress_video() {
    if [[ $# -lt 2 ]]; then
        echo "Usage: compress_video input_file output_file [quality]"
        echo "  input_file: Path to the input video file"
        echo "  output_file: Path to the output video file"
        echo "  quality: ultrafast, superfast, veryfast, faster, fast, medium, slow, slower, veryslow (default: medium)"
        return 1
    fi

    local file="$1"
    local output="$2"
    local quality="${3:-medium}"

    # Check if file exists
    if [[ ! -f "$file" ]]; then
        echo "Error: Input file '$file' does not exist"
        return 1
    fi

    echo "Processing file: $file"

    # Try with VideoToolbox hardware acceleration
    local command="ffmpeg -i \"$file\" -movflags +faststart -preset $quality -crf 30 -r 30 -c:a copy \"$output\""
    echo "Command: $command"

    if eval $command; then
        echo "Output file: $output"
    else
        echo "Hardware encoding failed. Falling back to software encoding."
        command="ffmpeg -i \"$file\" -movflags +faststart -preset $quality -crf 30 -r 30 -c:a copy \"$output\""
        echo "Command: $command"
        eval $command
        echo "Output file: $output"
    fi
}

# Function to render subtitles
render_subtitle() {
    # ---------------------------
    # 1. 定义默认值
    # ---------------------------
    local input_file=""
    local src_srt=""
    local trans_srt=""
    local output_file=""
    local target_width=1920
    local target_height=1080
    local quality="ultrafast"
    local subtitle_mode="both"

    # ---------------------------
    # 2. 解析命令行参数
    # ---------------------------
    while [[ $# -gt 0 ]]; do
        case "$1" in
            --src-srt=*)
                src_srt="${1#*=}"
                shift
                ;;
            --trans-srt=*)
                trans_srt="${1#*=}"
                shift
                ;;
            --output=*)
                output_file="${1#*=}"
                shift
                ;;
            --width=*)
                target_width="${1#*=}"
                shift
                ;;
            --height=*)
                target_height="${1#*=}"
                shift
                ;;
            --quality=*)
                quality="${1#*=}"
                shift
                ;;
            --subtitle-mode=*)
                subtitle_mode="${1#*=}"
                shift
                ;;
            -h|--help)
                echo "Usage: render_subtitle <input_file> [--src-srt=FILE] [--trans-srt=FILE] [--output=FILE]"
                echo "                              [--width=WIDTH] [--height=HEIGHT] [--quality=PRESET] [--subtitle-mode=MODE]"
                echo
                echo "  <input_file>               : 视频输入文件（必须是第一个非 --flag 参数）"
                echo "  --src-srt=FILE             : 原文字幕文件路径"
                echo "  --trans-srt=FILE           : 翻译字幕文件路径"
                echo "  --output=FILE              : 输出文件路径，默认为 input_file_name-渲染后.mp4"
                echo "  --width=WIDTH              : 目标宽度，默认 1920"
                echo "  --height=HEIGHT            : 目标高度，默认 1080"
                echo "  --quality=PRESET           : ffmpeg 预设（ultrafast, superfast, ... , medium, slow, etc），默认 medium"
                echo "  --subtitle-mode=MODE       : 渲染模式，可选 'both' (默认), 'src' (仅原文), 'trans' (仅翻译)"
                echo
                return 0
                ;;
            -*)
                echo "Unknown option: $1"
                return 1
                ;;
            *)
                # 如果 input_file 还没赋值，就把当前参数作为 input_file
                if [[ -z "$input_file" ]]; then
                    input_file="$1"
                else
                    echo "Unexpected argument: $1"
                    return 1
                fi
                shift
                ;;
        esac
    done

    # ---------------------------
    # 3. 检查必需参数
    # ---------------------------
    if [[ -z "$input_file" ]]; then
        echo "Error: No input video file specified."
        echo "Try '--help' for more information."
        return 1
    fi

    if [[ ! -f "$input_file" ]]; then
        echo "Error: Input file '$input_file' does not exist"
        return 1
    fi

    # 如果没有指定 output 文件，就自动生成一个带后缀的
    if [[ -z "$output_file" ]]; then
        # 去掉扩展名后再加后缀
        local base_name="${input_file%.*}"
        output_file="${base_name}-rendered.mp4"
    fi

    echo "Input file      : $input_file"
    echo "Output file     : $output_file"
    echo "Subtitle mode   : $subtitle_mode"
    echo "Target width    : $target_width"
    echo "Target height   : $target_height"
    echo "Quality preset  : $quality"

    # ---------------------------
    # 4. 字幕样式设置
    # ---------------------------
    local src_font_color='&HFFFFFF'
    local src_outline_color='&H000000'
    local src_outline_width=1
    local src_shadow_color='&H80000000'

    local trans_font_color='&H00FFFE'
    local trans_outline_color='&H000000'
    local trans_outline_width=1
    local trans_back_color='&H33000000'
    local trans_border_style=1

    local src_font_size=15
    local trans_font_size=17
    local font_name='Arial'
    local trans_font_name='LXGW WenKai'

    # ---------------------------
    # 5. 构造 filter_graph
    # ---------------------------
    # 首先做缩放 + 填充，让视频满足 target_width x target_height 大小
    local base_filter="scale=${target_width}:${target_height}:force_original_aspect_ratio=decrease,pad=${target_width}:${target_height}:(ow-iw)/2:(oh-ih)/2"
    local filter_graph=""

    case "$subtitle_mode" in
        both)
            # 需要确保 src_srt 和 trans_srt 文件都给定了
            if [[ ! -f "$src_srt" ]]; then
                echo "Error: For 'both' mode, --src-srt must be specified and exist."
                return 1
            fi
            if [[ ! -f "$trans_srt" ]]; then
                echo "Error: For 'both' mode, --trans-srt must be specified and exist."
                return 1
            fi
            filter_graph="${base_filter},subtitles=${src_srt}:force_style='FontSize=${src_font_size},FontName=${font_name},PrimaryColour=${src_font_color},OutlineColour=${src_outline_color},OutlineWidth=${src_outline_width},ShadowColour=${src_shadow_color},BorderStyle=1',subtitles=${trans_srt}:force_style='FontSize=${trans_font_size},FontName=${trans_font_name},PrimaryColour=${trans_font_color},OutlineColour=${trans_outline_color},OutlineWidth=${trans_outline_width},BackColour=${trans_back_color},Alignment=2,MarginV=27,ShadowColour=${src_shadow_color},BorderStyle=${trans_border_style}'"
            ;;
        src)
            # 仅原文字幕
            if [[ ! -f "$src_srt" ]]; then
                echo "Error: --src-srt must be specified and exist for 'src' mode."
                return 1
            fi
            filter_graph="${base_filter},subtitles=${src_srt}:force_style='FontSize=${src_font_size},FontName=${font_name},PrimaryColour=${src_font_color},OutlineColour=${src_outline_color},OutlineWidth=${src_outline_width},ShadowColour=${src_shadow_color},BorderStyle=1'"
            ;;
        trans)
            # 仅翻译字幕
            if [[ ! -f "$trans_srt" ]]; then
                echo "Error: --trans-srt must be specified and exist for 'trans' mode."
                return 1
            fi
            filter_graph="${base_filter},subtitles=${trans_srt}:force_style='FontSize=${trans_font_size},FontName=${trans_font_name},PrimaryColour=${trans_font_color},OutlineColour=${trans_outline_color},OutlineWidth=${trans_outline_width},BackColour=${trans_back_color},Alignment=2,MarginV=27,ShadowColour=${src_shadow_color},BorderStyle=${trans_border_style}'"
            ;;
        *)
            echo "Error: Unknown subtitle_mode '$subtitle_mode'. Use 'both', 'src', or 'trans'."
            return 1
            ;;
    esac

    echo "Filter graph    : $filter_graph"

    # ---------------------------
    # 8. 尝试硬件加速 (VideoToolbox)，失败则回退软件编码
    # ---------------------------
    if ffmpeg -hide_banner -encoders 2>&1 | grep -q "h264_videotoolbox"; then
        echo "Attempting VideoToolbox hardware encoding..."
        if ffmpeg -i "$input_file" \
                   -vf "$filter_graph" \
                   -c:v h264_videotoolbox \
                   -preset "$quality" \
                   -c:a copy \
                   "$output_file"
        then
            echo "Hardware encoding successful."
        else
            echo "Hardware encoding failed. Falling back to software encoding..."
            ffmpeg -i "$input_file" \
                   -vf "$filter_graph" \
                   -c:v libx264 \
                   -preset "$quality" \
                   -c:a copy \
                   "$output_file"
        fi
    else
        echo "VideoToolbox not available. Using software encoding..."
        ffmpeg -i "$input_file" \
               -vf "$filter_graph" \
               -c:v libx264 \
               -preset "$quality" \
               -c:a copy \
               "$output_file"
    fi
}


# Function to clip an audio file
clip_audio() {
    if [[ $# -lt 3 ]]; then
        echo "Usage: clip_audio input_file output_file start_time [duration]"
        echo "  input_file: Path to the input audio file"
        echo "  output_file: Path to the output audio file"
        echo "  start_time: Start time in seconds or in HH:MM:SS format"
        echo "  duration: (Optional) Duration in seconds or in HH:MM:SS format"
        echo "            If not provided, will clip from start_time to the end of the file"
        return 1
    fi

    local input_file="$1"
    local output_file="$2"
    local start_time="$3"
    local duration="$4"

    # Check if input file exists
    if [[ ! -f "$input_file" ]]; then
        echo "Error: Input file '$input_file' does not exist"
        return 1
    fi

    # Get total duration of the input file
    local total_duration=$(ffprobe -v error -show_entries format=duration -of default=noprint_wrappers=1:nokey=1 "$input_file")
    if [[ -z "$total_duration" ]]; then
        echo "Error: Could not determine the duration of the input file"
        return 1
    fi

    # Convert start_time to seconds if in HH:MM:SS format
    if [[ "$start_time" == *":"* ]]; then
        local h m s
        IFS=: read -r h m s <<< "$start_time"
        start_time=$(( 10#$h * 3600 + 10#$m * 60 + 10#$s ))
    fi

    # Handle start time overflow
    if (( $(echo "$start_time > $total_duration" | bc -l) )); then
        echo "Error: Start time ($start_time seconds) exceeds the total duration of the file ($total_duration seconds)"
        return 1
    fi

    # Handle duration
    local end_flag=""
    if [[ -n "$duration" ]]; then
        # Convert duration to seconds if in HH:MM:SS format
        if [[ "$duration" == *":"* ]]; then
            local h m s
            IFS=: read -r h m s <<< "$duration"
            duration=$(( 10#$h * 3600 + 10#$m * 60 + 10#$s ))
        fi

        # Calculate end time
        local end_time=$(echo "$start_time + $duration" | bc -l)

        # Handle duration overflow
        if (( $(echo "$end_time > $total_duration" | bc -l) )); then
            echo "Warning: Requested end time ($end_time seconds) exceeds the total duration of the file ($total_duration seconds)"
            echo "Clipping from $start_time seconds to the end of the file"
        else
            end_flag="-t $duration"
        fi
    fi

    # Check if output file already exists
    if [[ -f "$output_file" ]]; then
        echo -n "Output file '$output_file' already exists. Do you want to overwrite it? (Y/N): "
        read -r overwrite
        if [[ "$overwrite" != "Y" && "$overwrite" != "y" ]]; then
            echo "Operation cancelled. File exists and overwrite denied."
            return 1
        fi
    fi

    echo "Clipping audio from $start_time seconds ${duration:+for $duration seconds }to '$output_file'"

    # Perform the clip
    local command="ffmpeg -i \"$input_file\" -ss $start_time $end_flag -c copy \"$output_file\""
    echo "Command: $command"

    if eval $command; then
        echo "Successfully clipped to: $output_file"
    else
        echo "Warning: Fast copy failed. Trying with re-encoding."
        # If copy codec fails, try with re-encoding
        command="ffmpeg -i \"$input_file\" -ss $start_time $end_flag \"$output_file\""
        echo "Command: $command"
        if eval $command; then
            echo "Successfully clipped to: $output_file"
        else
            echo "Error: Failed to clip audio file"
            return 1
        fi
    fi
}

# Usage examples
# extract_video_audio "input.mp4" "output"
# get_video_codec "input.mp4"
# convert_video "input.mp4" "output.mp4" "h264" "medium"
# compress_video "input.mp4" "output.mp4" "medium"
# render_subtitle "input.mp4" "en.srt" "fr.srt" "output.mp4" 1920 1080 "medium"
# clip_audio "input.mp3" "output.mp3" "00:01:30" "00:02:00"
