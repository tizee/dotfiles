#!/bin/bash

# Default values
COMMIT_COUNT=100
OUTPUT_DIR="./postmortem/"

# Parse arguments
while getopts ":c:o:" opt; do
  case $opt in
    c) COMMIT_COUNT="$OPTARG" ;;
    o) OUTPUT_DIR="$OPTARG" ;;
    \?) echo "Invalid option -$OPTARG" >&2; exit 1 ;;
  esac
done

# Create output directory
mkdir -p "$OUTPUT_DIR"

echo "Generating postmortems for latest $COMMIT_COUNT fix commits..."

# Fetch fix commits and generate basic postmortems
git log --grep="fix\|bug\|resolve" -i --format="%H %s" -n "$COMMIT_COUNT" | while IFS= read -r line; do
  commit_hash=$(echo "$line" | awk '{print $1}')
  commit_msg=$(echo "$line" | cut -d' ' -f2-)

  # Create report file
  report_file="$OUTPUT_DIR/postmortem_${commit_hash:0:8}.md"

  # Use template
  cp "$SKILL_ROOT/references/postmortem-template.md" "$report_file"

  # Replace placeholders with basic info
  sed -i "s/\[Bug Summary\]/$commit_msg/" "$report_file"
  sed -i "s/\[Critical\/High\/Medium\/Low\]/Medium/" "$report_file"
  sed -i "s/\[Brief description of what caused the bug\]/Needs analysis/" "$report_file"
  sed -i "s/\[commit-hash\]/$commit_hash/" "$report_file"
  sed -i "s/\[Short commit message\]/$commit_msg/" "$report_file"
  sed -i "s/\[fix-commit-hash\]/$commit_hash/" "$report_file"
  sed -i "s/\[Short fix commit message\]/$commit_msg/" "$report_file"
  sed -i "s/\[1-2 lessons learned from this bug\]/Needs analysis/" "$report_file"
  sed -i "s/\[How to avoid this bug in the future\]/Needs analysis/" "$report_file"
done

# Create simple index
echo "# Postmortem Index\n\nGenerated on: $(date)\nTotal reports: $COMMIT_COUNT\n" > "$OUTPUT_DIR/index.md"
ls "$OUTPUT_DIR" | grep -E "postmortem_.+\.md" | sort | while read -r file; do
  echo "- [${file%.md}](./$file)" >> "$OUTPUT_DIR/index.md"
done

echo "Done! Postmortems generated in $OUTPUT_DIR"
echo "Index: $OUTPUT_DIR/index.md"