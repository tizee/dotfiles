---
name: hackernews
description: Fetch and explore Hacker News content via the official API. Use when the user wants to browse top/best/new/ask/show/job stories, get story details with comments, look up user profiles, or check recently changed items. Triggers on "hacker news", "HN stories", "HN front page", "trending on HN", "show hn", "ask hn", or any request to fetch or summarize Hacker News content.
---

# Hacker News

Fetch stories, comments, and user profiles from the Hacker News API.

## Invocation

Always use `hn` directly. It is already installed on PATH.

```bash
hn stories --type top --limit 10
```

Only if `hn` is not found (command not found error), fall back to invoking the script from the skill directory:

```bash
scripts/hn stories --type top --limit 10
```

Do NOT use `scripts/hn` as the default invocation.

Default output is readable markdown. Add `--json` before the subcommand for structured JSON (for piping to `jq` or other tools).

## Commands

### stories

Fetch ranked story lists.

```
hn stories --type <type> --limit <N>
```

| `--type` | Content |
|----------|---------|
| `top` | Front page (default, includes jobs) |
| `new` | Newest stories |
| `best` | Highest-rated |
| `ask` | Ask HN |
| `show` | Show HN |
| `job` | Job listings |

`--limit` defaults to 10. API max: 500 for top/new/best, 200 for ask/show/job.

Output: numbered list with title, link, score, author, time, and comment count with HN discussion link.

### item

Fetch a single item (story, comment, job, poll).

```
hn item <id> [--with-comments] [--comment-depth N]
```

- `--with-comments`: recursively fetch the comment tree
- `--comment-depth N`: max recursion depth (default: 2). Keep low to avoid large fetches.

Without `--with-comments`, returns the item with comment count hint.

### user

Fetch a user profile.

```
hn user <username>
```

Returns karma, about, creation date, and the 20 most recent submission IDs.

### updates

Fetch recently changed items and profiles.

```
hn updates
```

## Setup (for reference only)

If `hn` is missing from PATH, ask the user to run:

```bash
ln -sf /Users/tizee/projects/project-conf/dotfiles/tizee-dotfiles/claude/skills/hackernews/scripts/hn /usr/local/bin/hn
```

Requires `uv`. The script manages its own dependencies (httpx) via inline metadata.

## Direct API Access

For queries the script does not cover, use curl or `WebFetch` against the HN Firebase API directly. See `references/api.md` for full endpoint documentation.

```bash
curl -s https://hacker-news.firebaseio.com/v0/maxitem.json
curl -s https://hacker-news.firebaseio.com/v0/item/8863.json
```

## Tips

- Comment trees can be large. Start with `--comment-depth 1` and increase if needed.
- The API has no rate limit, but batch fetching many items creates many HTTP requests. Keep `--limit` reasonable.
- Markdown output converts HTML to readable text (links preserved, tags stripped). Use `--json` for raw HTML fields.
- User `submitted` lists can contain thousands of IDs; the script returns only the 20 most recent.
