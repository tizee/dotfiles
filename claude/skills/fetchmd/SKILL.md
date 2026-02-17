---
name: fetchmd
description: Convert web pages to markdown using r.jina.ai API. Use when user wants to fetch a URL and save as markdown, extract content from web pages, or convert HTML to markdown. Triggered by phrases like "fetch url", "convert to markdown", "scrape webpage", or any request involving fetching web content.
---

# Fetchmd Skill

Use the `fetchmd` CLI tool to convert web pages to markdown.

## Setup

Ensure the user has a config file at `~/.config/fetchmd/config.json` with their Jina API key:

```json
{"auth_key": "your-jina-api-key"}
```

If config is missing, inform the user and ask them to create it.

## Basic Usage

```bash
fetchmd <URL> <output_file>
```

## Options

| Option | Description | Values |
|--------|-------------|--------|
| `--format` | Output format | `markdown` (default), `html`, `text`, `content`, `pageshot`, `screenshot`, `vlm`, `readerlm-v2` |
| `--retain-images` | Image retention | `all` (default), `none`, `alt`, `all_p`, `alt_p` |
| `--retain-links` | Link retention | `all` (default), `none`, `text`, `gpt-oss` |
| `--target-selector` | CSS selector for target element | - |
| `--remove-selector` | CSS selectors to remove (comma-separated) | - |
| `--wait-for-selector` | CSS selector to wait for before returning | - |
| `--timeout` | Request timeout in seconds (max 180) | 30 (default) |
| `--no-cache` | Ignore cache | flag |

## Examples

Fetch a simple page:
```bash
fetchmd https://example.com page.md
```

Extract specific content with CSS selector:
```bash
fetchmd https://example.com article.md --target-selector "main article"
```

Fetch as HTML instead of markdown:
```bash
fetchmd https://example.com output.html --format html
```

Wait for dynamic content (SPA):
```bash
fetchmd https://example.com page.md --wait-for-selector ".content-loaded"
```

Remove unwanted elements:
```bash
fetchmd https://example.com clean.md --remove-selector "nav, footer, .ads"
```
