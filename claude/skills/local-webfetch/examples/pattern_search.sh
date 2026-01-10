#!/bin/bash
# Pattern search example

echo "Searching for 'JavaScript' documentation..."

# Search multiple pages
urls=(
    "https://developer.mozilla.org/en-US/docs/Learn/JavaScript"
    "https://javascript.info"
    "https://www.w3schools.com/js/"
)

for url in "${urls[@]}"; do
    echo ""
    echo "=== $url ==="

    # Use rg to search directly
    result=$(playwrightmd "$url" 2>/dev/null | rg "JavaScript" -m 10 -n)

    if [ -n "$result" ]; then
        echo "Found JavaScript content:"
        echo "$result"
    else
        echo "No JavaScript content found (or page failed to load)"
    fi
done
