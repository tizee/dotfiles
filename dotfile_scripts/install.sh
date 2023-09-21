#!/usr/bin/env bash
# vim:ft=sh:foldmethod=marker

# ______
#/\__  _\__
#\/_/\ \/\_\  ____      __     __
#   \ \ \/\ \/\_ ,`\  /'__`\ /'__`\
#    \ \ \ \ \/_/  /_/\  __//\  __/
#     \ \_\ \_\/\____\ \____\ \____\
#      \/_/\/_/\/____/\/____/\/____/
#
# Tizee (Jeff Chiang)
# https://github.com/tizee

# failed on error
set -e
script_path="$(realpath $0)"
script_dir="$(dirname "$script_path")"

# ====================
# Color Output
# ====================
# {{{
reset_color='\033[0m'
lightgreen='\033[92m'
lightyellow='\033[38;5;228m'
red='\033[38;5;196m'
# }}}

# ====================
# Banner
# ====================
echo -e "${lightgreen}"
echo -e '               __              __                  '
echo -e '              /\ \            /\ \                 '
echo -e ' __  __    ___\ \ \___     ___\ \ \___      __     '
echo -e '/\ \/\ \  / __`\ \  _ `\  / __`\ \  _ `\  /"__`\   '
echo -e '\ \ \_\ \/\ \L\ \ \ \ \ \/\ \L\ \ \ \ \ \/\ \L\.\_ '
echo -e ' \/`____ \ \____/\ \_\ \_\ \____/\ \_\ \_\ \__/.\_\'
echo -e '  `/___/> \/___/  \/_/\/_/\/___/  \/_/\/_/\/__/\/_/'
echo -e '     /\___/                                        '
echo -e '     \/__/                                         '
echo -e "${reset_color}"


if [[ $(uname -s) = "Darwin" ]]; then
  "$script_dir/mac.sh"
  "$script_dir/mac_defaults.sh"
else
  "$script_dir/linux.sh"
fi
