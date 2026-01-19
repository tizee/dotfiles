#!/usr/bin/env zsh

# Guard: prevent reloading
(( ${+_FFMPEG_PLUGIN_LOADED} )) && return
typeset -g _FFMPEG_PLUGIN_LOADED=1

# ============================================================================
# Constants
# ============================================================================

typeset -r FFMPEG_DEFAULT_PRESET='medium'
typeset -r FFMPEG_DEFAULT_CRF=18
typeset -r FFMPEG_DEFAULT_AUDIO_RATE=16000
typeset -r FFMPEG_DEFAULT_AUDIO_CHANNELS=1

# ============================================================================
# Helper Functions
# ============================================================================

# Check if a file exists
# Returns 0 if exists, 1 otherwise
_ffmpeg_check_file() {
    [[ -f "$1" ]] && return 0
    print "Error: File '$1' does not exist" >&2
    return 1
}

# Check if VideoToolbox encoder is available
# Returns 0 if available, 1 otherwise
_ffmpeg_is_videotoolbox_available() {
    local codec="${1:-h264}"
    ffmpeg -hide_banner -encoders 2>&1 | command grep -q "${codec}_videotoolbox"
}

# Convert time (HH:MM:SS or seconds) to seconds
_ffmpeg_time_to_seconds() {
    local time="$1"
    if [[ "$time" == *":"* ]]; then
        local h m s
        IFS=: read -r h m s <<< "$time"
        print $(( 10#$h * 3600 + 10#$m * 60 + 10#$s ))
    else
        print "$time"
    fi
}

# ============================================================================
# Audio Functions
# ============================================================================

# Extract audio from video file to MP3 format
# Usage: get_simple_audio <input_file> <output_name>
get_simple_audio() {
    (( $# < 2 )) && {
        print "Usage: get_simple_audio <input_file> <output_name>" >&2
        return 1
    }

    local input="$1"
    local output="$2"

    _ffmpeg_check_file "$input" || return 1

    # Create output directory if needed
    local output_dir="${output:h}"
    [[ "$output_dir" != "." && ! -d "$output_dir" ]] && command mkdir -p "$output_dir"

    ffmpeg -v error \
        -i "$input" \
        -ar "$FFMPEG_DEFAULT_AUDIO_RATE" \
        -ac "$FFMPEG_DEFAULT_AUDIO_CHANNELS" \
        -map 0:a \
        -c:a mp3 \
        "${output}.mp3" || return 1

    print "Conversion complete: ${output}.mp3"
}

# Extract audio from video to AAC format
# Usage: extract_video_audio <input_file> <output_file>
extract_video_audio() {
    (( $# < 2 )) && {
        print "Usage: extract_video_audio <input_file> <output_file>" >&2
        print "  input_file: Path to the input video file" >&2
        print "  output_file: Path to the output audio file (without extension)" >&2
        return 1
    }

    _ffmpeg_check_file "$1" || return 1

    ffmpeg -i "$1" -c:a aac -q:a 2 "${2}.m4a"
}

# Clip an audio/video file
# Usage: clip_audio <input_file> <output_file> <start_time> [duration]
#   start_time: Seconds or HH:MM:SS format
#   duration: (Optional) Seconds or HH:MM:SS format
clip_audio() {
    (( $# < 3 )) && {
        print "Usage: clip_audio <input_file> <output_file> <start_time> [duration]" >&2
        print "  start_time: Start time in seconds or HH:MM:SS format" >&2
        print "  duration: (Optional) Duration in seconds or HH:MM:SS format" >&2
        return 1
    }

    local input="$1" output="$2" start_time="$3" duration="$4"

    _ffmpeg_check_file "$input" || return 1

    # Get total duration
    local total_duration
    total_duration=$(ffprobe -v error -show_entries format=duration \
        -of default=noprint_wrappers=1:nokey=1 "$input")

    [[ -z "$total_duration" ]] && {
        print "Error: Could not determine input file duration" >&2
        return 1
    }

    # Convert times to seconds
    start_time=$(_ffmpeg_time_to_seconds "$start_time")
    [[ -n "$duration" ]] && duration=$(_ffmpeg_time_to_seconds "$duration")

    # Validate start time
    (( start_time > total_duration )) && {
        print "Error: Start time ($start_time) exceeds total duration ($total_duration)" >&2
        return 1
    }

    # Handle duration
    local duration_flag=""
    if [[ -n "$duration" ]]; then
        local end_time=$(( start_time + duration ))
        if (( end_time > total_duration )); then
            print "Warning: End time ($end_time) exceeds total duration ($total_duration)" >&2
            print "Clipping from $start_time to end of file" >&2
        else
            duration_flag="-t $duration"
        fi
    fi

    # Check for existing output
    if [[ -f "$output" ]]; then
        print -n "Output file '$output' exists. Overwrite? (y/N): " >&2
        read -r overwrite
        [[ "$overwrite" != "y" && "$overwrite" != "Y" ]] && {
            print "Operation cancelled" >&2
            return 1
        }
    fi

    print "Clipping $input -> $output (from ${start_time}s ${duration:+for ${duration}s })" >&2

    # Try stream copy first
    if ffmpeg -i "$input" -ss "$start_time" $duration_flag -c copy "$output" 2>/dev/null; then
        print "Success: $output" >&2
        return 0
    fi

    # Fall back to re-encoding
    print "Stream copy failed, trying re-encoding..." >&2
    if ffmpeg -i "$input" -ss "$start_time" $duration_flag "$output"; then
        print "Success: $output" >&2
        return 0
    fi

    print "Error: Failed to clip file" >&2
    return 1
}

# ============================================================================
# Encoder Info Functions
# ============================================================================

# Get all encoders in JSON format
ffmpeg_encoders_json() {
    local output
    output=$(ffmpeg -hide_banner -encoders </dev/null 2>&1)

    local -a video_encoders=() audio_encoders=() subtitle_encoders=()
    local found_separator=false

    local -a lines=("${(@f)output}")

    for line in "${lines[@]}"; do
        local trimmed="${line##[[:space:]]}"
        trimmed="${trimmed%%[[:space:]]}"

        [[ "$found_separator" == "false" ]] && {
            [[ "$trimmed" =~ ^------ ]] && found_separator=true
            continue
        }

        local -a fields=("${(@s: :)trimmed}")
        (( ${#fields[@]} < 2 )) && continue

        local code="${fields[1]}" encoder="${fields[2]}"

        case "${code:0:1}" in
            V) video_encoders+=("$encoder") ;;
            A) audio_encoders+=("$encoder") ;;
            S) subtitle_encoders+=("$encoder") ;;
        esac
    done

    jq -n \
        --argjson video "$(printf '%s\n' "${video_encoders[@]}" | jq -R . | jq -s .)" \
        --argjson audio "$(printf '%s\n' "${audio_encoders[@]}" | jq -R . | jq -s .)" \
        --argjson subtitle "$(printf '%s\n' "${subtitle_encoders[@]}" | jq -R . | jq -s .)" \
        '{video_encoder: $video, audio_encoder: $audio, subtitle_encoder: $subtitle}'
}

# Get video encoder list
get_video_encoder_list() {
    ffmpeg_encoders_json | jq '.video_encoder'
}

# Get audio encoder list
get_audio_encoder_list() {
    ffmpeg_encoders_json | jq '.audio_encoder'
}

# Search for encoders by name
# Usage: ffmpeg_search_encoder <encoder_name> [--audio]
ffmpeg_search_encoder() {
    (( $# < 1 )) && {
        print "Usage: ffmpeg_search_encoder <encoder_name> [--audio]" >&2
        return 1
    }

    local encoder="$1" audio=false
    [[ "$2" == "--audio" ]] && audio=true

    if [[ "$audio" == "true" ]]; then
        get_audio_encoder_list | jq -r ".[] | select(contains(\"${encoder}\"))"
    else
        get_video_encoder_list | jq -r ".[] | select(contains(\"${encoder}\"))"
    fi
}

# ============================================================================
# Video Info Functions
# ============================================================================

# Get video information in JSON format
# Usage: get_video_info <input_file>
get_video_info() {
    (( $# < 1 )) && {
        print "Usage: get_video_info <input_file>" >&2
        return 1
    }

    _ffmpeg_check_file "$1" || return 1

    ffprobe -v error -select_streams v:0 \
        -show_entries stream=codec_name,codec_long_name,profile,codec_type,codec_tag_string,codec_tag,width,height,coded_width,coded_height,display_aspect_ratio \
        -of json "$1"
}

# Get video codec information
# Usage: get_video_codec <input_file>
get_video_codec() {
    (( $# < 1 )) && {
        print "Usage: get_video_codec <input_file>" >&2
        return 1
    }

    _ffmpeg_check_file "$1" || return 1

    get_video_info "$1" | jq '.streams[] | {codec_name, codec_long_name, codec_tag_string}'
}

# ============================================================================
# Video Conversion Functions
# ============================================================================

# Convert video with optional hardware acceleration
# Usage: convert_video <input> <output> [format] [preset] [crf]
#   format: h264 or h265 (default: h264)
#   preset: ultrafast~veryslow (default: veryslow)
#   crf: 0-51 (default: 18, lower = better quality)
convert_video() {
    (( $# < 2 )) && {
        print "Usage: convert_video <input> <output> [format] [preset] [crf]" >&2
        print "  format: h264 or h265 (default: h264)" >&2
        print "  preset: ultrafast~veryslow (default: veryslow)" >&2
        print "  crf: 0-51 (default: 18, lower = better quality)" >&2
        return 1
    }

    local input="$1" output="$2"
    local format="${3:-h264}"
    local preset="${4:-veryslow}"
    local crf="${5:-$FFMPEG_DEFAULT_CRF}"

    _ffmpeg_check_file "$input" || return 1

    print "Converting: $input -> $output" >&2
    print "Format: $format, Preset: $preset, CRF: $crf" >&2

    # Check existing output
    if [[ -f "$output" ]]; then
        print -n "Output exists. Overwrite? (y/N): " >&2
        read -r overwrite
        [[ "$overwrite" != "y" && "$overwrite" != "Y" ]] && {
            print "Cancelled" >&2
            return 1
        }
    fi

    # Determine codec
    local sw_codec hw_codec
    case "$format" in
        h265|hevc) sw_codec="libx265"; hw_codec="hevc" ;;
        *)          sw_codec="libx264"; hw_codec="h264" ;;
    esac

    # Try hardware encoding
    if _ffmpeg_is_videotoolbox_available "$hw_codec"; then
        print "Trying hardware encoding..." >&2
        if ffmpeg -i "$input" \
            -c:v "${hw_codec}_videotoolbox" \
            -q:v "$crf" \
            -preset "$preset" \
            -c:a copy \
            "$output" 2>/dev/null; then
            print "Hardware encoding successful" >&2
            return 0
        fi
        print "Hardware encoding failed, falling back to software..." >&2
    fi

    # Software encoding
    print "Software encoding: $sw_codec CRF=$crf preset=$preset" >&2
    ffmpeg -i "$input" \
        -c:v "$sw_codec" \
        -preset "$preset" \
        -crf "$crf" \
        -c:a copy \
        "$output"
}

# Compress video for web sharing
# Usage: compress_video <input> <output> [preset]
compress_video() {
    (( $# < 2 )) && {
        print "Usage: compress_video <input> <output> [preset]" >&2
        return 1
    }

    local input="$1" output="$2" preset="${3:-$FFMPEG_DEFAULT_PRESET}"

    _ffmpeg_check_file "$input" || return 1

    print "Compressing: $input -> $output" >&2

    ffmpeg -i "$input" \
        -movflags +faststart \
        -preset "$preset" \
        -crf 30 \
        -r 30 \
        -c:a copy \
        "$output"
}

# ============================================================================
# Subtitle Rendering
# ============================================================================

# Render subtitles into video
# Usage: render_subtitle <input> [options]
#   --src-srt=FILE        Source subtitle file
#   --trans-srt=FILE      Translation subtitle file
#   --output=FILE         Output file (default: <input>-rendered.mp4)
#   --width=WIDTH         Target width (default: 1920)
#   --height=HEIGHT       Target height (default: 1080)
#   --quality=PRESET       FFmpeg preset (default: medium)
#   --subtitle-mode=MODE  both|src|trans (default: both)
render_subtitle() {
    local input="" src_srt="" trans_srt="" output_file=""
    local width=1920 height=1080
    local quality="medium" mode="both"

    # Parse arguments
    while (( $# > 0 )); do
        case "$1" in
            --src-srt=*)      src_srt="${1#*=}" ;;
            --trans-srt=*)    trans_srt="${1#*=}" ;;
            --output=*)       output_file="${1#*=}" ;;
            --width=*)        width="${1#*=}" ;;
            --height=*)       height="${1#*=}" ;;
            --quality=*)      quality="${1#*=}" ;;
            --subtitle-mode=*) mode="${1#*=}" ;;
            -h|--help)
                print "Usage: render_subtitle <input> [options]" >&2
                print "  --src-srt=FILE        Source subtitle file" >&2
                print "  --trans-srt=FILE      Translation subtitle file" >&2
                print "  --output=FILE         Output file" >&2
                print "  --width=WIDTH         Target width (default: 1920)" >&2
                print "  --height=HEIGHT       Target height (default: 1080)" >&2
                print "  --quality=PRESET       FFmpeg preset (default: medium)" >&2
                print "  --subtitle-mode=MODE  both|src|trans (default: both)" >&2
                return 0
                ;;
            -*)
                print "Unknown option: $1" >&2
                return 1
                ;;
            *)
                [[ -z "$input" ]] && input="$1" || {
                    print "Unexpected argument: $1" >&2
                    return 1
                }
                ;;
        esac
        shift
    done

    [[ -z "$input" ]] && {
        print "Error: No input file specified" >&2
        return 1
    }

    _ffmpeg_check_file "$input" || return 1

    # Default output
    [[ -z "$output_file" ]] && output_file="${input%.*}-rendered.mp4"

    # Validate subtitle files based on mode
    case "$mode" in
        both)
            _ffmpeg_check_file "$src_srt" || return 1
            _ffmpeg_check_file "$trans_srt" || return 1
            ;;
        src)
            _ffmpeg_check_file "$src_srt" || return 1
            ;;
        trans)
            _ffmpeg_check_file "$trans_srt" || return 1
            ;;
        *)
            print "Error: Invalid mode '$mode'. Use: both, src, or trans" >&2
            return 1
            ;;
    esac

    # Build filter graph
    local base_filter="scale=${width}:${height}:force_original_aspect_ratio=decrease,pad=${width}:${height}:(ow-iw)/2:(oh-ih)/2"
    local filter_graph

    # Subtitle styles
    local src_style='FontSize=15,FontName=Arial,PrimaryColour=&HFFFFFF,OutlineColour=&H000000,OutlineWidth=1,ShadowColour=&H80000000,BorderStyle=1'
    local trans_style='FontSize=17,FontName=LXGW WenKai,PrimaryColour=&H00FFFE,OutlineColour=&H000000,OutlineWidth=1,BackColour=&H33000000,Alignment=2,MarginV=27,ShadowColour=&H80000000,BorderStyle=1'

    case "$mode" in
        both)
            filter_graph="${base_filter},subtitles=${src_srt}:force_style='${src_style}',subtitles=${trans_srt}:force_style='${trans_style}'"
            ;;
        src)
            filter_graph="${base_filter},subtitles=${src_srt}:force_style='${src_style}'"
            ;;
        trans)
            filter_graph="${base_filter},subtitles=${trans_srt}:force_style='${trans_style}'"
            ;;
    esac

    print "Rendering: $input -> $output_file" >&2

    # Try hardware encoding first
    if _ffmpeg_is_videotoolbox_available h264; then
        print "Trying hardware encoding..." >&2
        if ffmpeg -i "$input" -vf "$filter_graph" \
            -c:v h264_videotoolbox -preset "$quality" -c:a copy "$output_file" 2>/dev/null; then
            print "Done" >&2
            return 0
        fi
        print "Hardware failed, using software..." >&2
    fi

    # Software encoding
    ffmpeg -i "$input" -vf "$filter_graph" \
        -c:v libx264 -preset "$quality" -c:a copy "$output_file"
}
