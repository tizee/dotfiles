---
name: local-webfetch
description: This skill should be used when the user asks to "fetch a webpage", "download HTML", "scrape content", "get website content", "render JavaScript page", or similar requests for web content retrieval. Provides best practices for AI agents using playwrightmd.
version: 0.1.0
---

# Local Webfetch Skill

## Overview
Use this skill to fetch web content using the `playwrightmd` tool, a specialized HTML-to-Markdown downloader that can render JavaScript-rendered content. This skill provides AI agents with flexible, token-aware strategies and the autonomy to choose the best approach for each situation.

## Core Principles for AI Agents

When retrieving web content, use your judgment to choose the most efficient approach based on the page size, user needs, and context constraints. You have full autonomy to decide which strategy to use:

### Flexible Retrieval Strategies
Choose from these approaches based on the situation:

```bash
# Option 1: Return full content directly (best for small to medium pages)
playwrightmd https://example.com

# Option 2: Filter content before returning (focus on relevant parts)
playwrightmd https://example.com | rg "Important Section" -A 10 -B 2

# Option 3: Use temporary files for large pages or multiple processing passes
tmp_file=$(mktemp -t page.XXXXXX.md)
playwrightmd https://example.com -o "$tmp_file"
cat "$tmp_file"

# Option 4: Provide summary or key sections for very large pages
playwrightmd https://example.com | rg -E '(# |## )' | head -50  # Show document structure
```

### Key Considerations
- **User intent**: If the user wants the full page content, return it directly when feasible
- **Context limits**: For very large pages, consider returning only relevant sections or a summary
- **Efficiency**: Use filtering tools to avoid sending unnecessary tokens to the user
- **Flexibility**: There's no single "right" approach - adapt to each unique situation

### Filtering and Processing Techniques
Use these techniques to process content efficiently when needed:

```bash
# Get specific sections with rg (ripgrep)
playwrightmd https://docs.python.org | rg "tutorial" -A 2 -B 2

# Use htmlq for HTML parsing (raw mode)
playwrightmd https://example.com --raw | htmlq "article h2"

# Search for patterns before downloading
playwrightmd https://example.com | grep -q "important pattern" && echo "Page contains pattern"
```

### Handling Dynamic/JavaScript-rendered Content
Use raw mode to get full HTML after JavaScript execution when needed:

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
- Use judgment to choose the right approach for each situation
- Consider user intent when deciding how much content to return
- Use filtering tools to focus on relevant content when appropriate
- Use `--raw` mode to get raw HTML content for dynamic pages
- Validate content before processing if needed

**DON'T**:
- Rigidly follow workflows without considering context
- Overly restrict content unless context limits require it
- Assume all pages are static HTML
- Ignore token limits when handling very large content
