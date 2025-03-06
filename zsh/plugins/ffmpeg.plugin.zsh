#!/usr/bin/env zsh

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
    # Capture ffmpeg output
    local ffmpeg_output=$(ffmpeg -hide_banner -encoders 2>&1)

    # Initialize arrays for encoder types
    local video_encoders=()
    local audio_encoders=()
    local subtitle_encoders=()

    local found_separator=false

    # Process each line of output
    while IFS= read -r line; do
        local trimmed_line=$(echo "$line" | xargs)

        if [[ "$found_separator" == "false" ]]; then
            if [[ "$trimmed_line" =~ ^------ ]]; then
                found_separator=true
            fi
            continue
        fi

        # Split line into components using read
        local code encoder rest
        read -r code encoder rest <<< "$trimmed_line"

        if [[ -z "$encoder" ]]; then
            continue
        fi

        # Determine encoder type from first character of code
        case "${code:0:1}" in
            V) video_encoders+=("$encoder") ;;
            A) audio_encoders+=("$encoder") ;;
            S) subtitle_encoders+=("$encoder") ;;
        esac
    done <<< "$ffmpeg_output"

    # Create JSON output using jq
    local json_output=$(jq -n \
        --argjson video_encoder "$(printf '%s\n' "${video_encoders[@]}" | jq -R . | jq -s .)" \
        --argjson audio_encoder "$(printf '%s\n' "${audio_encoders[@]}" | jq -R . | jq -s .)" \
        --argjson subtitle_encoder "$(printf '%s\n' "${subtitle_encoders[@]}" | jq -R . | jq -s .)" \
        '{video_encoder: $video_encoder, audio_encoder: $audio_encoder, subtitle_encoder: $subtitle_encoder}')

    echo "$json_output"
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

# Function to convert video with hardware acceleration when available
convert_video() {
    if [[ $# -lt 2 ]]; then
        echo "Usage: convert_video input_file output_file [format] [quality]"
        echo "  input_file: Path to the input video file"
        echo "  output_file: Path to the output video file"
        echo "  format: h264 or h265 (default: h264)"
        echo "  quality: ultrafast, superfast, veryfast, faster, fast, medium, slow, slower, veryslow (default: medium)"
        return 1
    fi

    local file="$1"
    local output="$2"
    local format="${3:-h264}"
    local quality="${4:-medium}"

    # Check if file exists
    if [[ ! -f "$file" ]]; then
        echo "Error: Input file '$file' does not exist"
        return 1
    fi

    echo "Processing file: $file"
    echo "Output file: $output"

    # Check if output file already exists
    if [[ -f "$output" ]]; then
        echo -n "Output file '$output' already exists. Do you want to overwrite it? (Y/N): "
        read -r overwrite
        if [[ "$overwrite" != "Y" && "$overwrite" != "y" ]]; then
            echo "Operation cancelled. File exists and overwrite denied."
            return 1
        fi
    fi

    # Set codec based on format
    local codec=""
    if [[ "$format" == "h265" ]]; then
        codec="hevc"
    else
        codec="h264"
    fi

    # Check if VideoToolbox (macOS hardware acceleration) is available
    local hw_available=false
    if ffmpeg -hide_banner -encoders 2>&1 | grep -q "${codec}_videotoolbox"; then
        hw_available=true
    fi

    if [[ "$hw_available" == "true" ]]; then
        echo "Using VideoToolbox hardware acceleration."
        # Try hardware encoding
        if ffmpeg -i "$file" -c:v ${codec}_videotoolbox -preset $quality -c:a copy "$output"; then
            echo "Hardware encoding successful."
        else
            echo "Hardware encoding failed. Falling back to software encoding."
            # Fallback to software encoding
            local sw_codec="libx264"
            [[ "$format" == "h265" ]] && sw_codec="libx265"
            ffmpeg -i "$file" -c:v $sw_codec -preset $quality -c:a copy "$output"
        fi
    else
        echo "VideoToolbox hardware acceleration not available. Using software encoding."
        # Software encoding
        local sw_codec="libx264"
        [[ "$format" == "h265" ]] && sw_codec="libx265"
        ffmpeg -i "$file" -c:v $sw_codec -preset $quality -c:a copy "$output"
    fi
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
    local command="ffmpeg -i \"$file\" -tag:v avc1 -c:v h264_videotoolbox -movflags +faststart -preset $quality -crf 30 -c:a copy \"$output\""
    echo "Command: $command"

    if eval $command; then
        echo "Output file: $output"
    else
        echo "Hardware encoding failed. Falling back to software encoding."
        command="ffmpeg -i \"$file\" -tag:v avc1 -c:v libx264 -movflags +faststart -preset $quality -crf 30 -c:a copy \"$output\""
        echo "Command: $command"
        eval $command
        echo "Output file: $output"
    fi
}

# Function to render subtitles
render_subtitle() {
    if [[ $# -lt 4 ]]; then
        echo "Usage: render_subtitle input_file src_srt trans_srt output_file [target_width] [target_height] [quality]"
        echo "  input_file: Path to the input video file"
        echo "  src_srt: Path to the source language subtitle file"
        echo "  trans_srt: Path to the translated language subtitle file"
        echo "  output_file: Path to the output video file"
        echo "  target_width: Video width (default: 1920)"
        echo "  target_height: Video height (default: 1080)"
        echo "  quality: ultrafast, superfast, veryfast, faster, fast, medium, slow, slower, veryslow (default: medium)"
        return 1
    fi

    local file="$1"
    local src_srt="$2"
    local trans_srt="$3"
    local output="$4"
    local target_width="${5:-1920}"
    local target_height="${6:-1080}"
    local quality="${7:-medium}"

    # Check if file exists
    if [[ ! -f "$file" ]]; then
        echo "Error: Input file '$file' does not exist"
        return 1
    fi

    # Check if subtitle files exist
    if [[ ! -f "$src_srt" ]]; then
        echo "Error: Source subtitle file '$src_srt' does not exist"
        return 1
    fi

    if [[ ! -f "$trans_srt" ]]; then
        echo "Error: Translated subtitle file '$trans_srt' does not exist"
        return 1
    fi

    echo "Processing file: $file"
    echo "Output file: $output"

    # Font and color settings
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

    # Create filter graph for subtitles
    local filter_graph="scale=${target_width}:${target_height}:force_original_aspect_ratio=decrease,pad=${target_width}:${target_height}:(ow-iw)/2:(oh-ih)/2,subtitles=${src_srt}:force_style='FontSize=${src_font_size},FontName=${font_name},PrimaryColour=${src_font_color},OutlineColour=${src_outline_color},OutlineWidth=${src_outline_width},ShadowColour=${src_shadow_color},BorderStyle=1',subtitles=${trans_srt}:force_style='FontSize=${trans_font_size},FontName=${trans_font_name},PrimaryColour=${trans_font_color},OutlineColour=${trans_outline_color},OutlineWidth=${trans_outline_width},BackColour=${trans_back_color},Alignment=2,MarginV=27,ShadowColour=${src_shadow_color},BorderStyle=${trans_border_style}'"

    echo "Filter graph: $filter_graph"

    # Check if output file already exists
    if [[ -f "$output" ]]; then
        echo -n "Output file '$output' already exists. Do you want to overwrite it? (Y/N): "
        read -r overwrite
        if [[ "$overwrite" != "Y" && "$overwrite" != "y" ]]; then
            echo "Operation cancelled. File exists and overwrite denied."
            return 1
        fi
    fi

    # Try with VideoToolbox hardware acceleration
    if ffmpeg -i "$file" -c:v h264_videotoolbox -vf "$filter_graph" -preset $quality "$output"; then
        echo "Hardware encoding successful."
    else
        echo "Hardware encoding failed. Falling back to software encoding."
        ffmpeg -i "$file" -c:v libx264 -vf "$filter_graph" -preset $quality "$output"
    fi
}

# Usage examples
# extract_video_audio "input.mp4" "output"
# get_video_codec "input.mp4"
# convert_video "input.mp4" "output.mp4" "h264" "medium"
# compress_video "input.mp4" "output.mp4" "medium"
# render_subtitle "input.mp4" "en.srt" "fr.srt" "output.mp4" 1920 1080 "medium"
