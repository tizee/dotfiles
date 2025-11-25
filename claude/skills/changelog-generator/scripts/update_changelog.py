#!/usr/bin/env python3
"""
Update CHANGELOG.md with new version entries.

Automates the process of inserting new changelog entries into an existing CHANGELOG.md file.
"""

import argparse
import re
import sys
from datetime import datetime
from pathlib import Path
from typing import List, Optional


def read_file(file_path: Path) -> str:
    """Read file contents."""
    try:
        return file_path.read_text()
    except FileNotFoundError:
        print(f"Error: File not found: {file_path}", file=sys.stderr)
        sys.exit(1)


def write_file(file_path: Path, content: str) -> None:
    """Write content to file."""
    file_path.write_text(content)


def find_unreleased_section(lines: List[str]) -> Optional[int]:
    """Find the line number of the [Unreleased] section."""
    for i, line in enumerate(lines):
        if re.match(r"^##\s+\[Unreleased\]", line):
            return i
    return None


def find_first_version_section(lines: List[str]) -> Optional[int]:
    """Find the line number of the first version section."""
    for i, line in enumerate(lines):
        # Match version pattern like [1.0.0] or [v1.0.0]
        if re.match(r"^##\s+\[v?\d+\.\d+\.\d+\]", line):
            return i
    return None


def create_version_section(version: str, date: str, entries: str) -> str:
    """Create a new version section."""
    lines = [
        f"## [{version}] - {date}",
        "",
        entries.rstrip(),
        "",
    ]
    return "\n".join(lines)


def update_unreleased_links(content: str, new_version: str, repo_url: Optional[str] = None) -> str:
    """Update comparison links for unreleased and new version."""
    if not repo_url:
        return content

    # Update [Unreleased] link to compare against new version
    content = re.sub(
        r'\[Unreleased\]:.*',
        f'[Unreleased]: {repo_url}/compare/v{new_version}...HEAD',
        content
    )

    # Add link for new version (assuming previous version exists)
    # This is a simplified approach - you may need to customize based on your repo structure
    lines = content.split('\n')
    for i, line in enumerate(lines):
        if line.startswith('[Unreleased]:'):
            # Insert new version link after Unreleased
            new_link = f'[{new_version}]: {repo_url}/releases/tag/v{new_version}'
            lines.insert(i + 1, new_link)
            break

    return '\n'.join(lines)


def insert_version_section(
    changelog_path: Path,
    version: str,
    entries: str,
    date: Optional[str] = None,
    repo_url: Optional[str] = None,
) -> None:
    """Insert a new version section into CHANGELOG.md."""
    # Use current date if not specified
    if not date:
        date = datetime.now().strftime("%Y-%m-%d")

    # Read existing changelog
    content = read_file(changelog_path)
    lines = content.split("\n")

    # Create new version section
    version_section = create_version_section(version, date, entries)

    # Find insertion point
    unreleased_idx = find_unreleased_section(lines)
    first_version_idx = find_first_version_section(lines)

    if unreleased_idx is not None:
        # Insert after [Unreleased] section
        # Find the end of [Unreleased] section (next ## heading or first version)
        insert_idx = first_version_idx if first_version_idx else len(lines)

        # Find blank line before next section
        for i in range(unreleased_idx + 1, insert_idx):
            if lines[i].startswith("##"):
                insert_idx = i
                break

    elif first_version_idx is not None:
        # No [Unreleased], insert before first version
        insert_idx = first_version_idx

    else:
        # No existing versions, insert after header
        # Find the end of the header (after format/versioning links)
        insert_idx = 0
        for i, line in enumerate(lines):
            if line.startswith("#") and i > 0:
                insert_idx = i
                break
        if insert_idx == 0:
            insert_idx = len(lines)

    # Insert new section
    lines.insert(insert_idx, version_section)

    # Join and write
    new_content = "\n".join(lines)

    # Update links if repo URL provided
    if repo_url:
        new_content = update_unreleased_links(new_content, version, repo_url)

    write_file(changelog_path, new_content)


def main():
    parser = argparse.ArgumentParser(
        description="Update CHANGELOG.md with a new version section"
    )

    parser.add_argument(
        "version",
        help="Version number (e.g., 1.2.0)",
    )

    parser.add_argument(
        "entries",
        help="Path to file containing changelog entries (markdown format)",
    )

    parser.add_argument(
        "--changelog",
        default="CHANGELOG.md",
        help="Path to CHANGELOG.md file (default: CHANGELOG.md)",
    )

    parser.add_argument(
        "--date",
        help="Release date in YYYY-MM-DD format (default: today)",
    )

    parser.add_argument(
        "--repo-url",
        help="Repository URL for generating comparison links",
    )

    args = parser.parse_args()

    # Validate version format
    if not re.match(r"^\d+\.\d+\.\d+$", args.version):
        print("Error: Version must be in semver format (e.g., 1.2.0)", file=sys.stderr)
        sys.exit(1)

    # Read entries
    entries_path = Path(args.entries)
    entries = read_file(entries_path)

    # Update changelog
    changelog_path = Path(args.changelog)

    if not changelog_path.exists():
        print(f"Error: CHANGELOG file not found: {changelog_path}", file=sys.stderr)
        print("Tip: Create one from the template first", file=sys.stderr)
        sys.exit(1)

    insert_version_section(
        changelog_path,
        args.version,
        entries,
        date=args.date,
        repo_url=args.repo_url,
    )

    print(f"✓ Updated {changelog_path} with version {args.version}")


if __name__ == "__main__":
    main()
