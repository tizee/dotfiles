---
name: rss-reader
description: Fetch and manage RSS/Atom feed subscriptions stored in OPML format. Use when the user wants to list RSS subscriptions, add/remove RSS feeds, fetch and read latest articles, search across feeds, get a daily digest, or export feed content in markdown or JSON. Triggers on "rss", "rss feeds", "subscriptions", "fetch articles", "news feeds", "atom feed", "opml", "what's new", "digest", "search feeds", or any request to read, browse, search, or manage RSS content.
---

# RSS Reader

Manage RSS/Atom subscriptions (OPML) and fetch feed content as markdown or JSON.

## Invocation

Always use `rss` directly. It is already installed on PATH.

```bash
rss list
```

Only if `rss` is not found (command not found error), fall back to the skill directory:

```bash
scripts/rss list
```

Do NOT use `scripts/rss` as the default invocation.

Default output is readable markdown. Add `--json` before the subcommand for structured JSON.

## Configuration

- **OPML file**: `~/.config/llm/rss-reader.opml` (auto-created on first use)
- **Cache dir**: `~/.cache/rss-reader/` (cached XML, 30-min TTL)
- Override via env: `RSS_OPML_PATH`, `RSS_CACHE_DIR`

## Commands

### list

List all RSS subscriptions, grouped by category.

```
rss list
```

### add / remove

```
rss add <url> [--title TITLE] [--category CATEGORY]
rss remove <query>
```

`add` is a no-op if the URL already exists. `remove` matches by title or URL substring.

### merge

Import feeds from another OPML file. Deduplicates by `xmlUrl`.

```
rss merge <file>
```

### recategorize

Move a feed to a different category.

```
rss recategorize <query> --category CATEGORY
```

### fetch

Fetch and display feed content. Progressive disclosure: summaries by default.

```
rss fetch <query> [--limit N] [--refresh] [--full] [--index N]
rss fetch --all [--limit N] [--refresh]
```

| Flag | Description |
|------|-------------|
| `--all` | Fetch all subscribed feeds |
| `--limit N` | Max items per feed (default: 10) |
| `--refresh` | Bypass cache and re-fetch from source |
| `--full` | Show full article content instead of summary |
| `--index N` | Show a specific item by 1-based index (always full content) |

### search

Search articles across ALL subscribed feeds by keyword.

```
rss search <keyword> [--limit N] [--refresh] [--feeds "rust,simon"]
```

Matches against title, description, and full content. Results sorted by date (newest first). Default limit: 20.

Cache-first: scans already-cached XML instantly. Only fetches uncached feeds (with `--refresh`, re-fetches all).

### digest

Show recent articles across all feeds within a time window.

```
rss digest [--since 24h] [--limit N] [--refresh] [--feeds "rust,simon"]
```

| Flag | Description |
|------|-------------|
| `--since` | Time window: `24h`, `7d`, `30m`, etc. (default: 24h) |
| `--limit N` | Max results (default: 50) |
| `--feeds` | Comma-separated filter by feed title/URL substring |

Results grouped by feed, sorted newest first. Cache-first like `search`.

## Agent Workflows

### Answering "what's new?"

```bash
rss digest --since 24h
```

Then summarize the output for the user, highlighting items relevant to their interests.

### Finding information on a topic

```bash
rss search "rust" --limit 10
```

Scan results, then use `rss fetch <feed> --index N` to drill into specific articles.

### Auto-categorizing feeds

When feeds lack categories (e.g. after a bulk `merge`):

1. `rss --json list` to get all feeds with their titles/URLs
2. Analyze the feed titles and URLs to infer topics (e.g. "simonwillison.net" -> Tech/AI, "krebsonsecurity.com" -> Security)
3. `rss recategorize <query> --category <CATEGORY>` for each feed

This leverages the agent's own reasoning rather than hardcoding classification rules.

### Curated briefing

Combine search + digest for a focused briefing:

```bash
rss digest --since 7d --limit 100
rss search "LLM" --limit 10
rss search "rust" --limit 10
```

Synthesize findings into a summary for the user.

## Setup

If `rss` is missing from PATH, ask the user to symlink:

```bash
ln -sf /Users/tizee/projects/project-conf/dotfiles/tizee-dotfiles/claude/skills/rss-reader/scripts/rss /usr/local/bin/rss
```

Requires `uv`. The script manages its own dependencies (httpx, defusedxml, html-to-markdown) via inline metadata.

## Tips

- Use `--json` for piping to `jq` or further processing.
- Cache is per-feed URL, stored under `~/.cache/rss-reader/`. Use `--refresh` to force re-fetch.
- The OPML file is standard format -- importable/exportable with any RSS reader.
- Full `content:encoded` is rendered when available; falls back to `description` summary.
- `search` and `digest` fetch all feeds, which can be slow with many subscriptions. Use cached results when possible.
