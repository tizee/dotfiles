#!/usr/bin/env zsh
# Simple benchmark script to measure plugin loading time

echo "Benchmarking plugin loading time..."
echo

# Method 1: Time the plugin loading section
time_start=$(date +%s%N)

for file in $HOME/.config/zsh/plugins/*.plugin.zsh(N) $HOME/.config/zsh/widgets/*.widget.zsh(N); do
  builtin source "$file"
done

time_end=$(date +%s%N)
time_diff=$(( (time_end - time_start) / 1000000 ))

echo "Plugin loading time: ${time_diff}ms"
echo

# Show which plugins are loaded
echo "Loaded plugins:"
typeset -g | grep '_LOADED=1' | sed 's/=1$//' | sed 's/^/  - /'
