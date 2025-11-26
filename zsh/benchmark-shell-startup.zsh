#!/usr/bin/env zsh
# Benchmark shell startup time
# Usage: ./benchmark-shell-startup.zsh [runs]

# Number of runs (default: 10)
runs=${1:-10}

echo "=== Shell Startup Benchmark ==="
echo "Shell: $SHELL"
echo "Runs: $runs"
echo

# Arrays to store results
typeset -a times_ms
total_time=0

# Perform benchmark runs
for ((i=1; i<=runs; i++)); do
  # Capture time output, extract total time in seconds
  time_output=$( (time $SHELL -i -c exit) 2>&1 )

  # Extract time from output (format: "0.051 total" or "cpu 0.051 total")
  time_seconds=$(echo "$time_output" | grep -E 'total$' | awk '{print $(NF-1)}')

  # Convert to milliseconds
  time_ms=$(printf "%.0f" $(echo "$time_seconds * 1000" | bc))

  times_ms+=($time_ms)
  total_time=$((total_time + time_ms))

  printf "Run %2d: %3dms\n" $i $time_ms
done

# Calculate statistics
average=$((total_time / runs))

# Find min and max
min=${times_ms[1]}
max=${times_ms[1]}
for time in $times_ms; do
  ((time < min)) && min=$time
  ((time > max)) && max=$time
done

# Calculate median
typeset -a sorted_times
sorted_times=(${(n)times_ms})  # numeric sort
if ((runs % 2 == 0)); then
  # Even number: average of middle two
  mid1=$((runs / 2))
  mid2=$((mid1 + 1))
  median=$(( (sorted_times[mid1] + sorted_times[mid2]) / 2 ))
else
  # Odd number: middle value
  mid=$(( (runs + 1) / 2 ))
  median=${sorted_times[mid]}
fi

echo
echo "=== Results ==="
printf "Average:  %3dms\n" $average
printf "Median:   %3dms\n" $median
printf "Min:      %3dms\n" $min
printf "Max:      %3dms\n" $max
printf "Range:    %3dms\n" $((max - min))

# Performance rating
echo
echo "=== Performance Rating ==="
if ((average < 50)); then
  echo "⚡ Excellent (< 50ms)"
elif ((average < 100)); then
  echo "✓ Good (50-100ms)"
elif ((average < 200)); then
  echo "○ Acceptable (100-200ms)"
else
  echo "⚠ Slow (> 200ms)"
fi
