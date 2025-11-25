#!/usr/bin/env python3
"""
Analyze git commits and generate changelog entries.

Supports conventional commit format and categorizes changes by type.
"""

import argparse
import json
import re
import subprocess
import sys
from collections import defaultdict
from datetime import datetime
from typing import Dict, List, Optional, Tuple


# Commit type to changelog category mapping
COMMIT_CATEGORIES = {
    "feat": "Added",
    "fix": "Fixed",
    "docs": "Documentation",
    "style": "Styles",
    "refactor": "Changed",
    "perf": "Performance",
    "test": "Tests",
    "build": "Build",
    "ci": "CI/CD",
    "chore": "Chores",
}

# Standard changelog section order
SECTION_ORDER = [
    "Added",
    "Changed",
    "Deprecated",
    "Removed",
    "Fixed",
    "Security",
    "Performance",
    "Documentation",
    "Tests",
    "Build",
    "CI/CD",
    "Chores",
    "Styles",
]


def run_git_command(args: List[str]) -> str:
    """Run a git command and return output."""
    try:
        result = subprocess.run(
            ["git"] + args,
            capture_output=True,
            text=True,
            check=True,
        )
        return result.stdout.strip()
    except subprocess.CalledProcessError as e:
        print(f"Git command failed: {e.stderr}", file=sys.stderr)
        sys.exit(1)


def get_last_tag() -> Optional[str]:
    """Get the most recent git tag."""
    try:
        return run_git_command(["describe", "--tags", "--abbrev=0"])
    except:
        return None


def get_commits(from_ref: str, to_ref: str = "HEAD") -> List[str]:
    """Get commit hashes in the specified range."""
    commit_range = f"{from_ref}..{to_ref}" if from_ref else to_ref
    output = run_git_command(["log", "--pretty=format:%H", commit_range])
    return output.split("\n") if output else []


def parse_commit(commit_hash: str) -> Dict:
    """Parse a single commit and extract metadata."""
    # Get commit message
    message = run_git_command(["log", "-1", "--pretty=format:%s", commit_hash])
    body = run_git_command(["log", "-1", "--pretty=format:%b", commit_hash])

    # Parse conventional commit format: type(scope): description
    pattern = r"^(\w+)(\(([^)]+)\))?(!)?:\s*(.+)$"
    match = re.match(pattern, message)

    if match:
        commit_type = match.group(1)
        scope = match.group(3) or ""
        breaking = bool(match.group(4))
        description = match.group(5)
    else:
        # Not conventional format, treat as "chore"
        commit_type = "chore"
        scope = ""
        breaking = False
        description = message

    # Check for breaking changes in body
    if not breaking and "BREAKING CHANGE:" in body:
        breaking = True

    return {
        "hash": commit_hash,
        "type": commit_type,
        "scope": scope,
        "description": description,
        "breaking": breaking,
        "message": message,
        "body": body,
    }


def strip_emoji(text: str) -> str:
    """Remove emoji characters from text."""
    # Emoji pattern (basic coverage)
    emoji_pattern = re.compile(
        "["
        u"\U0001F600-\U0001F64F"  # emoticons
        u"\U0001F300-\U0001F5FF"  # symbols & pictographs
        u"\U0001F680-\U0001F6FF"  # transport & map symbols
        u"\U0001F1E0-\U0001F1FF"  # flags
        u"\U00002702-\U000027B0"
        u"\U000024C2-\U0001F251"
        "]+",
        flags=re.UNICODE,
    )
    return emoji_pattern.sub("", text).strip()


def categorize_commits(
    commits: List[Dict],
    exclude_types: Optional[List[str]] = None,
    strip_emojis: bool = False,
) -> Dict[str, List[Dict]]:
    """Categorize commits by changelog section."""
    exclude_types = exclude_types or []
    categories = defaultdict(list)

    for commit in commits:
        # Skip excluded types
        if commit["type"] in exclude_types:
            continue

        # Determine category
        if commit["breaking"]:
            category = "Breaking Changes"
        else:
            category = COMMIT_CATEGORIES.get(commit["type"], "Other")

        # Process description
        description = commit["description"]
        if strip_emojis:
            description = strip_emoji(description)

        # Add scope if present
        if commit["scope"]:
            description = f"**{commit['scope']}**: {description}"

        categories[category].append({
            "description": description,
            "hash": commit["hash"][:7],  # Short hash
            "breaking": commit["breaking"],
        })

    return dict(categories)


def format_markdown(categories: Dict[str, List[Dict]], include_hashes: bool = False) -> str:
    """Format categorized commits as markdown."""
    lines = []

    # Sort sections by standard order
    sorted_categories = sorted(
        categories.items(),
        key=lambda x: SECTION_ORDER.index(x[0]) if x[0] in SECTION_ORDER else 999,
    )

    for category, entries in sorted_categories:
        if not entries:
            continue

        lines.append(f"### {category}")
        lines.append("")

        for entry in entries:
            marker = "- ⚠️ " if entry["breaking"] else "- "
            description = entry["description"]

            if include_hashes:
                description += f" ({entry['hash']})"

            lines.append(f"{marker}{description}")

        lines.append("")

    return "\n".join(lines)


def format_json(categories: Dict[str, List[Dict]]) -> str:
    """Format categorized commits as JSON."""
    return json.dumps(categories, indent=2)


def main():
    parser = argparse.ArgumentParser(
        description="Analyze git commits and generate changelog entries"
    )

    # Commit range options
    range_group = parser.add_mutually_exclusive_group()
    range_group.add_argument(
        "--since-last-tag",
        action="store_true",
        help="Analyze commits since the last git tag",
    )
    range_group.add_argument(
        "--from",
        dest="from_ref",
        help="Starting commit/tag reference",
    )

    parser.add_argument(
        "--to",
        dest="to_ref",
        default="HEAD",
        help="Ending commit/tag reference (default: HEAD)",
    )

    # Filtering options
    parser.add_argument(
        "--exclude-types",
        help="Comma-separated list of commit types to exclude (e.g., chore,style)",
    )

    # Formatting options
    parser.add_argument(
        "--format",
        choices=["markdown", "json"],
        default="markdown",
        help="Output format (default: markdown)",
    )
    parser.add_argument(
        "--strip-emojis",
        action="store_true",
        help="Remove emoji characters from output",
    )
    parser.add_argument(
        "--include-hashes",
        action="store_true",
        help="Include short commit hashes in output",
    )

    args = parser.parse_args()

    # Determine commit range
    if args.since_last_tag:
        last_tag = get_last_tag()
        if not last_tag:
            print("Error: No tags found in repository", file=sys.stderr)
            sys.exit(1)
        from_ref = last_tag
        print(f"Analyzing commits since {last_tag}...", file=sys.stderr)
    elif args.from_ref:
        from_ref = args.from_ref
        print(f"Analyzing commits from {from_ref} to {args.to_ref}...", file=sys.stderr)
    else:
        print("Error: Must specify --since-last-tag or --from", file=sys.stderr)
        parser.print_help()
        sys.exit(1)

    # Get commits
    commit_hashes = get_commits(from_ref, args.to_ref)

    if not commit_hashes:
        print("No commits found in range", file=sys.stderr)
        sys.exit(0)

    print(f"Found {len(commit_hashes)} commits", file=sys.stderr)

    # Parse commits
    commits = [parse_commit(h) for h in commit_hashes]

    # Categorize
    exclude_types = args.exclude_types.split(",") if args.exclude_types else None
    categories = categorize_commits(
        commits,
        exclude_types=exclude_types,
        strip_emojis=args.strip_emojis,
    )

    # Format output
    if args.format == "json":
        output = format_json(categories)
    else:
        output = format_markdown(categories, include_hashes=args.include_hashes)

    print(output)


if __name__ == "__main__":
    main()
