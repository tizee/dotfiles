#!/bin/bash
# Basic fetch example

# Download documentation page
url="https://docs.python.org/3/tutorial/introduction.html"
tmp_file=$(mktemp -t python-docs.XXXXXX.md)

echo "Fetching $url..."
playwrightmd "$url" -o "$tmp_file"

echo ""
echo "--- Download Complete ---"
wc -l "$tmp_file"
echo ""
echo "First 50 lines:"
head -50 "$tmp_file"
echo ""
echo "To read more: less \"$tmp_file\""
echo "To extract sections: rg 'strings' -A 10 -B 2 \"$tmp_file\""
