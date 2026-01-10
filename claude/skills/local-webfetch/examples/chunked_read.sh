#!/bin/bash
# Chunked reading example for large pages

# Download large documentation site
url="https://platform.openai.com/docs/api-reference"
tmp_file=$(mktemp -t openai-docs.XXXXXX.md)

echo "Fetching large page..."
playwrightmd "$url" -o "$tmp_file"

echo ""
echo "--- Page Analysis ---"
total_lines=$(wc -l < "$tmp_file")
total_words=$(wc -w < "$tmp_file")
echo "Total lines: $total_lines"
echo "Total words: $total_words"

# Decide how to present
if [ "$total_lines" -gt 200 ]; then
    echo ""
    echo "--- Too large for full display ---"
    echo ""
    echo "Table of Contents:"
    grep "^# " "$tmp_file" | head -20
    echo ""
    echo "--- First 100 lines ---"
    head -100 "$tmp_file"
    echo ""
    echo "--- To find specific content:"
    echo "rg 'chat completion' \"$tmp_file\" -A 10"
    echo "sed -n '500,600p' \"$tmp_file\""
else
    echo ""
    echo "--- Full Content ---"
    cat "$tmp_file"
fi
