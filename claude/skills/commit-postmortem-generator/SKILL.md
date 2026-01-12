---
name: commit-postmortem-generator
description: This skill should be used when the user asks to "analyze fix commits and generate postmortems", "create postmortems from git history", or "summarize bug fixes into postmortem reports".
version: 0.1.0
---

# Commit Postmortem Generator

## Core Purpose
Analyze fix commits in a git repository and generate concise postmortem reports that capture critical information about bugs and their fixes.

## Basic Workflow
To generate postmortems:
1. Fetch the latest N fix commits (default: 100)
2. Group commits by related issues
3. Generate markdown postmortem reports with key details
4. Store reports in ./postmortem/ directory

## Usage

### When the user doesn't specify a number of commits
1. Prompt the user: "How many latest fix commits would you like to analyze?"
2. Use their response to run the script:
```bash
./scripts/generate-postmortems.sh -c [user-specified-number]
```

### When the user specifies a number
```bash
# Example: Analyze 50 commits
./scripts/generate-postmortems.sh -c 50

# With custom output directory
./scripts/generate-postmortems.sh -c 50 -o ./my-postmortems/
```

## Resources
- **`references/postmortem-template.md`** - Minimal postmortem template
- **`scripts/generate-postmortems.sh`** - Main script to generate postmortems