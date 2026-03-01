# Hacker News API Reference

Base URL: `https://hacker-news.firebaseio.com/v0`

All endpoints return JSON. Append `?print=pretty` for formatted output.

## Items

`GET /item/<id>.json`

Items cover stories, comments, jobs, Ask HN, polls, and poll options.

| Field | Description |
|-------|-------------|
| **id** | Unique integer ID |
| type | `"story"`, `"comment"`, `"job"`, `"poll"`, or `"pollopt"` |
| by | Author username |
| time | Unix timestamp |
| title | Title (stories, polls, jobs). HTML |
| url | Story URL |
| text | Body text. HTML |
| score | Score / votes |
| descendants | Total comment count (stories/polls) |
| kids | Child comment IDs (ranked order) |
| parent | Parent item ID (comments) |
| deleted | `true` if deleted |
| dead | `true` if dead |
| parts | Related pollopt IDs (polls) |
| poll | Parent poll ID (pollopts) |

## Users

`GET /user/<username>.json`

Case-sensitive usernames. Only publicly active users are available.

| Field | Description |
|-------|-------------|
| **id** | Username |
| **created** | Unix timestamp |
| **karma** | Karma score |
| about | Self-description. HTML |
| submitted | List of story/poll/comment IDs |

## Story Lists

| Endpoint | Description | Max |
|----------|-------------|-----|
| `/topstories.json` | Top stories (includes jobs) | 500 |
| `/newstories.json` | Newest stories | 500 |
| `/beststories.json` | Best stories | 500 |
| `/askstories.json` | Ask HN | 200 |
| `/showstories.json` | Show HN | 200 |
| `/jobstories.json` | Job listings | 200 |

All return arrays of item IDs.

## Other Endpoints

| Endpoint | Description |
|----------|-------------|
| `/maxitem.json` | Current largest item ID |
| `/updates.json` | Recently changed items and profiles (`{items: [...], profiles: [...]}`) |
