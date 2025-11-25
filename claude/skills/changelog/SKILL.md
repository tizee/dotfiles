---
name: changelog
description: Generates and updates CHANGELOG.md files by analyzing git commit history. Handles both creating new changelogs and appending recent changes. Analyzes commits using conventional commit format and categorizes changes. Use this skill when generating a new CHANGELOG.md file, updating CHANGELOG.md with recent changes, preparing for a release, documenting changes between versions, or when user mentions "update changelog", "generate changelog", "changelog", "release notes".
---

# Changelog Management

Generates and updates CHANGELOG.md by analyzing git commit history with support for conventional commits and semantic versioning. Works with both new and existing changelog files.

## Usage Scenarios

This skill supports two distinct workflows:

### Scenario A: Preparing a Release

**When**: You are ready to create a new version release (e.g., v2.1.0)

**Process**:
1. Analyze commits since last release
2. Review and edit generated entries
3. Use `update_changelog.py` to automatically insert a new version section
4. Commit and tag the release

**Result**: CHANGELOG.md gets a new version section like `## [2.1.0] - 2025-11-25`

### Scenario B: Daily Development (No Release Yet)

**When**: You want to document recent changes but are not ready to release

**Process**:
1. Analyze recent commits
2. Review and edit generated entries
3. **Manually** update the `[Unreleased]` section in CHANGELOG.md
4. Do NOT use `update_changelog.py` (that's only for releases)

**Result**: CHANGELOG.md's `[Unreleased]` section gets updated with new changes

**Important**: For daily development, always edit CHANGELOG.md manually to add entries under `[Unreleased]`. The `update_changelog.py` script is only for creating formal release versions.

## Quick Start

Generate changelog for commits since last tag:

```bash
python scripts/analyze_commits.py --since-last-tag > new_entries.md
```

Or specify a commit range:

```bash
python scripts/analyze_commits.py --from=v1.0.0 --to=HEAD
```

## Workflow

Copy this checklist and track your progress:

```
Changelog Generation:
- [ ] Step 1: Identify version range
- [ ] Step 2: Analyze commits
- [ ] Step 3: Review and categorize
- [ ] Step 4: Update CHANGELOG.md
- [ ] Step 5: Verify formatting
```

### Step 1: Identify version range

Determine which commits to include:

**For new release**: Use commits since last tag
```bash
git describe --tags --abbrev=0  # Get last tag
```

**For specific range**: Specify commit range
```bash
git log --oneline <from-ref>..<to-ref>
```

### Step 2: Analyze commits

Run the analysis script:

```bash
python scripts/analyze_commits.py --since-last-tag --format=markdown
```

This outputs categorized changes by type:
- **Features** (feat:)
- **Bug Fixes** (fix:)
- **Documentation** (docs:)
- **Refactoring** (refactor:)
- **Performance** (perf:)
- **Tests** (test:)
- **Build/Chores** (build:, chore:, ci:)
- **Styles** (style:)

### Step 3: Review and categorize

Review the generated output:
- Verify commit categorization is correct
- Edit descriptions for clarity
- Combine related changes if needed
- Remove internal/non-user-facing changes

### Step 4: Update CHANGELOG.md

**First time setup**: If CHANGELOG.md doesn't exist, create it from template:

```bash
cp templates/changelog_template.md CHANGELOG.md
```

**Choose your approach based on scenario**:

#### For Release (Scenario A): Use update_changelog.py

When preparing a formal release, use the automation script:

```bash
# Save generated entries to a file
python scripts/analyze_commits.py --since-last-tag > /tmp/new_entries.md

# Review and edit /tmp/new_entries.md as needed

# Use script to insert new version section
python scripts/update_changelog.py 2.1.0 /tmp/new_entries.md
```

The script automatically:
- Inserts new version section with date
- Places it after `[Unreleased]` or before first version
- Updates comparison links (if `--repo-url` provided)

**Format** (automatically created by script):
```markdown
## [2.1.0] - 2025-11-25

### Added
- New feature descriptions

### Fixed
- Bug fix descriptions
```

#### For Daily Development (Scenario B): Manual Edit

When documenting changes without releasing, **manually edit** CHANGELOG.md:

1. Open CHANGELOG.md in your editor
2. Locate the `[Unreleased]` section
3. Add the generated entries under appropriate subsections
4. Keep existing unreleased entries if any

**Format** (manual editing):
```markdown
## [Unreleased]

### Added
- New feature from today's commits
- Previous unreleased feature (keep this)

### Fixed
- Bug fix from today's commits
```

**Important**: Do NOT use `update_changelog.py` for daily development. That script is ONLY for creating versioned releases.

### Step 5: Verify formatting

Check the updated CHANGELOG.md:
- Date format is YYYY-MM-DD
- Sections are in standard order
- Links work (if using reference-style)
- Formatting is consistent

## Commit Message Conventions

This skill expects **Conventional Commits** format:

```
<type>(<scope>): <description>

[optional body]

[optional footer]
```

**Recognized types**:
- `feat`: New feature
- `fix`: Bug fix
- `docs`: Documentation changes
- `style`: Code style changes (formatting, etc.)
- `refactor`: Code refactoring
- `perf`: Performance improvements
- `test`: Test additions or changes
- `build`: Build system changes
- `ci`: CI/CD changes
- `chore`: Other changes (maintenance, etc.)

**Breaking changes**: Include `BREAKING CHANGE:` in commit footer or append an exclamation mark after the type (e.g., `feat!:`)

## Customization

### Filtering commits

Exclude certain commit types:

```bash
python scripts/analyze_commits.py --exclude-types=chore,style
```

### Custom categories

Edit `scripts/analyze_commits.py` to modify the `COMMIT_CATEGORIES` mapping.

### Include emojis

Many projects use emojis in commits. The script preserves them by default.

To strip emojis from changelog:

```bash
python scripts/analyze_commits.py --strip-emojis
```

## Output Formats

**Markdown** (default):
```bash
python scripts/analyze_commits.py --format=markdown
```

**JSON** (for programmatic use):
```bash
python scripts/analyze_commits.py --format=json
```

## Scripts Reference

### analyze_commits.py

Analyzes git commits and generates categorized changelog entries.

**Usage**:
```bash
python scripts/analyze_commits.py [OPTIONS]
```

**Required Options** (one of):
- `--since-last-tag`: Analyze commits since the most recent git tag
- `--from=<ref>`: Starting commit/tag reference (e.g., `v1.0.0`, `abc123`)

**Optional Arguments**:
- `--to=<ref>`: Ending commit/tag reference (default: `HEAD`)
- `--exclude-types=<types>`: Comma-separated list of commit types to exclude
  - Example: `--exclude-types=chore,style`
- `--format=<format>`: Output format, either `markdown` or `json` (default: `markdown`)
- `--strip-emojis`: Remove emoji characters from commit descriptions
- `--include-hashes`: Include short commit hashes (7 chars) in output

**Examples**:

```bash
# Analyze all commits since last tag
python scripts/analyze_commits.py --since-last-tag

# Analyze specific range
python scripts/analyze_commits.py --from=v1.0.0 --to=v2.0.0

# Exclude maintenance commits
python scripts/analyze_commits.py --since-last-tag --exclude-types=chore,style,test

# Include commit hashes for reference
python scripts/analyze_commits.py --from=v1.0.0 --include-hashes

# Clean output without emojis
python scripts/analyze_commits.py --since-last-tag --strip-emojis

# JSON output for automation
python scripts/analyze_commits.py --since-last-tag --format=json
```

**Output Format** (Markdown):
```markdown
### Added
- feature description
- another feature

### Fixed
- bug fix description
```

**Output Format** (JSON):
```json
{
  "Added": [
    {"description": "feature description", "hash": "abc1234", "breaking": false}
  ],
  "Fixed": [
    {"description": "bug fix description", "hash": "def5678", "breaking": false}
  ]
}
```

### update_changelog.py

Updates CHANGELOG.md file with a new version section.

**IMPORTANT - When to use this script**:
- ✅ **USE**: When preparing a formal release (Scenario A)
- ❌ **DO NOT USE**: For daily development without release (Scenario B)
- For daily development, manually edit the `[Unreleased]` section instead

**Usage**:
```bash
python scripts/update_changelog.py <version> <entries_file> [OPTIONS]
```

**Required Arguments**:
- `version`: Version number in semver format (e.g., `1.2.0`, `2.0.0-beta.1`)
- `entries_file`: Path to file containing markdown-formatted changelog entries

**Optional Arguments**:
- `--changelog=<path>`: Path to CHANGELOG.md file (default: `CHANGELOG.md`)
- `--date=<date>`: Release date in YYYY-MM-DD format (default: today's date)
- `--repo-url=<url>`: Repository URL for generating version comparison links

**Examples**:

```bash
# Basic usage with today's date
python scripts/update_changelog.py 2.1.0 /tmp/new_entries.md

# Specify custom date
python scripts/update_changelog.py 2.1.0 /tmp/new_entries.md --date=2025-11-25

# Include repository links
python scripts/update_changelog.py 2.1.0 /tmp/new_entries.md \
  --repo-url=https://github.com/user/repo

# Custom CHANGELOG location
python scripts/update_changelog.py 2.1.0 /tmp/new_entries.md \
  --changelog=docs/CHANGELOG.md

# Complete example with all options
python scripts/update_changelog.py 2.1.0 /tmp/new_entries.md \
  --changelog=CHANGELOG.md \
  --date=2025-11-25 \
  --repo-url=https://github.com/user/repo
```

**Behavior**:
- Inserts new version section after `[Unreleased]` section (if present)
- Or inserts before first existing version section
- Automatically updates comparison links if `--repo-url` provided
- Creates properly formatted version header with date

**Example entries_file format**:
```markdown
### Added
- New feature for users
- Another cool feature

### Fixed
- Critical bug fix
- Minor UI issue
```

## Example Usage

### Example 1: Preparing a Release (Scenario A)

You are ready to release v2.1.0 after accumulating several commits:

```bash
# Step 1: Analyze commits since last release
python scripts/analyze_commits.py --since-last-tag > /tmp/v2.1.0_entries.md

# Step 2: Review and edit /tmp/v2.1.0_entries.md
# - Fix typos, improve descriptions
# - Remove internal-only changes
# - Combine related commits

# Step 3: Use script to update CHANGELOG.md
python scripts/update_changelog.py 2.1.0 /tmp/v2.1.0_entries.md \
  --repo-url=https://github.com/user/repo

# Step 4: Review CHANGELOG.md to verify insertion

# Step 5: Commit the changelog
git add CHANGELOG.md
git commit -m "docs: update CHANGELOG for v2.1.0"

# Step 6: Create release tag
git tag -a v2.1.0 -m "Release v2.1.0"
git push origin v2.1.0
```

**Result**: CHANGELOG.md now has a new `## [2.1.0] - 2025-11-25` section.

### Example 2: Daily Development (Scenario B)

You made some commits today and want to document them, but not ready to release:

```bash
# Step 1: Analyze recent commits (last 5 commits)
git log --oneline -5  # Check what commits to include
python scripts/analyze_commits.py --from=HEAD~5 --to=HEAD > /tmp/recent_changes.md

# Step 2: Review /tmp/recent_changes.md
# Edit for clarity and relevance

# Step 3: Manually open CHANGELOG.md and edit [Unreleased] section
# DO NOT use update_changelog.py

# Step 4: Copy relevant entries from /tmp/recent_changes.md
#         Paste under [Unreleased] section in CHANGELOG.md

# Step 5: Commit the updated changelog
git add CHANGELOG.md
git commit -m "docs: update unreleased changes in CHANGELOG"
```

**Result**: CHANGELOG.md's `[Unreleased]` section is updated with new entries.

**Key difference**: No version section created, no script used, manual editing only.

## Templates

See [templates/changelog_template.md](templates/changelog_template.md) for the standard CHANGELOG structure.

## Tips

- **Run analysis before creating tag**: Easier to identify the commit range
- **Keep entries user-focused**: Rewrite technical commits for end-users
- **Group related changes**: Combine multiple commits about same feature
- **Use imperative mood**: "Add feature" not "Added feature"
- **Link to issues/PRs**: Include references when relevant
- **Mark breaking changes clearly**: Use dedicated section or ⚠️ marker
