---
name: local-webfetch
description: This skill should be used when the user asks to "fetch a webpage", "download HTML", "scrape content", "get website content", "render JavaScript page", or similar requests for web content retrieval. Provides best practices for AI agents using playwrightmd.
version: 0.1.0
---

# Local Webfetch Skill

## Overview
Use this skill to fetch web content using the `playwrightmd` tool, a specialized HTML-to-Markdown downloader that can render JavaScript-rendered content. This skill provides AI agents with efficient, token-aware workflows for handling web content.

## Core Principles for AI Agents

When retrieving web content, always follow these token-efficient workflows to avoid consuming excessive context:

### 1. Download to Temporary File (Recommended)
Never return large web pages directly. Always download to a temporary file first, then read in chunks:

```bash
# Download page content
tmp_file=$(mktemp -t page.XXXXXX.md)
playwrightmd https://example.com -o "$tmp_file"

# Check size first
lines=$(wc -l < "$tmp_file")
echo "Downloaded $lines lines to $tmp_file"

# Read in manageable chunks
if [ "$lines" -gt 100 ]; then
    echo "First 100 lines:"
    head -100 "$tmp_file"
    echo "
... Use head/tail/sed/grep to view more"
else
    cat "$tmp_file"
fi
```

### 2. Pipe to Filtering Tools
Process content directly with filtering tools to avoid loading full text:

```bash
# Get specific sections with rg (ripgrep)
playwrightmd https://docs.python.org | rg "tutorial" -A 2 -B 2

# Use htmlq for HTML parsing (raw mode)
playwrightmd https://example.com --raw | htmlq "article h2"

# Search for patterns before downloading
playwrightmd https://example.com | grep -q "important pattern" && echo "Page contains pattern"
```

### 3. Use Raw Mode for JavaScript-rendered Pages
Get full HTML after JavaScript execution when needed:

```bash
# Render SPA content
playwrightmd https://react-app.com --raw -o /tmp/spa.html

# Extract specific elements with pup
playwrightmd https://vue-app.com --raw | pup 'section.content'
```

## Command Reference

### Basic Usage
```bash
# Markdown output (default)
playwrightmd https://example.com -o output.md

# Raw HTML output
playwrightmd https://example.com --raw -o output.html

# Pipe to stdout
playwrightmd https://example.com | head -50
```

### Advanced Options
```bash
# Custom User-Agent
playwrightmd https://example.com --user-agent "AI Agent Bot/1.0"

# Use proxy
playwrightmd https://example.com --proxy-url "http://proxy:8080"

# Wait for specific element
playwrightmd https://dynamic-app.com --wait-for ".content-loaded"

# Visible browser (for debugging)
playwrightmd https://example.com --no-headless
```

## Common Workflows

### Workflow 1: Retrieve Documentation
```bash
# Download and summarize
tmp=$(mktemp)
playwrightmd https://platform.openai.com/docs/api-reference/chat -o "$tmp"

# Check size
wc -l "$tmp"

# Extract key sections
sed -n '1,50p' "$tmp"  # Introduction
rg 'Parameters' -A 20 "$tmp"  # Parameters section
```

### Workflow 2: Check Website Content
```bash
# Quick content check
playwrightmd https://example.com | head -30

# Search for specific information
playwrightmd https://news-site.com | grep -A 10 -B 2 "breaking news"

# Check page structure (raw mode)
playwrightmd https://example.com --raw | htmlq -C -t "body > *"
```

### Workflow 3: Compare Static vs Rendered Content
```bash
# Get static HTML
curl -s https://react-app.com -o /tmp/static.html

# Get rendered HTML
playwrightmd https://react-app.com --raw -o /tmp/rendered.html

# Compare sizes
du -h /tmp/static.html /tmp/rendered.html

# Check content differences
diff /tmp/static.html /tmp/rendered.html | head -20
```

## Error Handling

### Timeouts for Slow Pages
```bash
# Longer timeout for slow sites
playwrightmd https://slow-site.com --timeout 60000

# Fast mode for basic pages
playwrightmd https://static-site.com --wait-until domcontentloaded
```

### Cloudflare/Bot Detection
The tool automatically handles most bot detection with:
- Realistic user agent
- Browser fingerprint masking
- JavaScript rendering

```bash
# Force more realistic mode (visible browser)
playwrightmd https://protected-site.com --no-headless
```

## Additional Resources

### Reference Files
For detailed command documentation:
- **`examples/`** - Working example scripts

### Example Scripts
```bash
# Examples of automated workflows
$SKILL_DIR/examples/basic_fetch.sh
$SKILL_DIR/examples/chunked_read.sh
$SKILL_DIR/examples/pattern_search.sh
```

### Validation
To validate installation:
```bash
playwrightmd --version
playwrightmd https://example.com --raw | head -5
```

## Best Practices Summary

**DO**:
- Always download to temporary files first
- Use filtering tools (head, tail, rg, grep) before reading full content
- Use `--raw` mode to get raw html content if required
- Handle large pages with chunked reading
- Validate content before processing

**DON'T**:
- Return full pages directly to users
- Pipe large output without filtering
- Assume all pages are static HTML
- Ignore token limits when handling content
