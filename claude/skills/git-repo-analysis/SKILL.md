---
name: git-repo-analysis
description: Analyze git repositories and generate comprehensive markdown documentation for quick understanding. Use when user asks to "analyze this repo", "document the codebase", "explain how this project works", "generate repo docs", "understand this codebase", or wants documentation about project architecture and modules. Triggers on requests for codebase analysis, architecture documentation, module documentation, or project understanding.
---

# Git Repository Analysis

Generate markdown documentation for quick understanding of a git repository through systematic top-down analysis.

## Analysis Workflow

```
Phase 1: Repository Overview
         ↓
Phase 2: Architecture Discovery
         ↓
Phase 3: Module Analysis (top-down)
         ↓
Phase 4: Documentation Generation
```

## Phase 1: Repository Overview

Gather high-level repository context:

1. **Identify project type** - Check for indicator files:
   - `package.json` → Node.js/JavaScript
   - `Cargo.toml` → Rust
   - `go.mod` → Go
   - `pyproject.toml`/`setup.py` → Python
   - `pom.xml`/`build.gradle` → Java
   - `Gemfile` → Ruby

2. **Read key files** - In order:
   - README.md (project purpose, setup)
   - LICENSE (open source? proprietary?)
   - CONTRIBUTING.md (development workflow)
   - CHANGELOG.md (project history)

3. **Assess repository state**:
   ```bash
   git log --oneline -20          # Recent activity
   git shortlog -sn --max-count=5 # Top contributors
   git branch -a                  # Branch structure
   git tag -l --sort=-creatordate | head -5  # Recent releases
   ```

4. **Record findings** for overview section of documentation.

## Phase 2: Architecture Discovery

Map the high-level structure:

1. **Directory scan** - List top-level directories:
   ```bash
   ls -la
   fd -t d --max-depth 1
   ```

2. **Identify architectural layers** - Common patterns:

   | Directory | Likely Purpose |
   |-----------|---------------|
   | `src/`, `lib/` | Source code |
   | `cmd/`, `bin/` | Entry points / CLI |
   | `pkg/`, `internal/` | Go package structure |
   | `app/`, `apps/` | Application code |
   | `api/`, `routes/` | API definitions |
   | `models/`, `entities/` | Data models |
   | `services/`, `core/` | Business logic |
   | `utils/`, `helpers/` | Utilities |
   | `config/`, `settings/` | Configuration |
   | `tests/`, `__tests__/` | Test suites |
   | `docs/` | Documentation |
   | `scripts/` | Build/deploy scripts |

3. **Find entry points**:
   - Look for `main.*`, `index.*`, `app.*`, `server.*`
   - Check `package.json` scripts, `Makefile` targets
   - Identify CLI commands in `bin/`, `cmd/`

4. **Detect patterns and frameworks**:
   - MVC, MVVM, Clean Architecture
   - Monorepo vs single package
   - Microservices vs monolith
   - Framework-specific conventions (Rails, Django, Next.js, etc.)

## Phase 3: Module Analysis (Top-Down)

Analyze each module systematically from top to bottom:

### Step 3.1: Enumerate Modules

List all modules/packages to analyze:

```bash
# For JS/TS
fd -e ts -e tsx -e js -e jsx --max-depth 2 -t f | head -30

# For Python
fd -e py --max-depth 2 -t f | head -30

# For Go
fd -e go --max-depth 2 -t f | head -30

# General approach
fd -t f --max-depth 2 | head -40
```

### Step 3.2: Per-Module Analysis

For each significant module/directory:

1. **Purpose identification**:
   - Read module's README if present
   - Scan file names for intent
   - Read main/index file first few lines

2. **Public interface discovery**:
   - Find exported functions/classes/types
   - Identify main entry points
   - Note configuration options

3. **Dependency mapping**:
   - Internal imports (other modules in project)
   - External dependencies
   - Data flow direction

4. **Key implementation details**:
   - Core algorithms or logic
   - State management approach
   - Error handling patterns
   - Important data structures

### Step 3.3: Cross-Module Relationships

Map how modules interact:

```
Use ast-grep or ripgrep to trace:
- Import/require statements
- Function calls across modules
- Shared types/interfaces
- Event emissions and subscriptions
```

## Phase 4: Documentation Generation

Generate structured markdown documentation.

### Output Structure

Create documentation with this hierarchy:

```
analysis-docs/
├── README.md              # Quick start and overview
├── ARCHITECTURE.md        # System design and patterns
├── modules/
│   ├── module-a.md        # Per-module documentation
│   ├── module-b.md
│   └── ...
└── diagrams/              # Optional: architecture diagrams
```

### Template: README.md (Quick Overview)

```markdown
# {Project Name}

> {One-line description from original README or inferred}

## What This Project Does

{2-3 sentence explanation of purpose and main functionality}

## Quick Start

{Essential commands to get running - copied/adapted from source}

## Project Structure

{High-level directory overview with purpose of each}

## Key Concepts

{3-5 core concepts needed to understand the codebase}

## Where to Start Reading

{Recommended entry points for understanding the code}
```

### Template: ARCHITECTURE.md

```markdown
# Architecture Overview

## System Design

{Describe the overall architecture pattern (MVC, microservices, etc.)}

## Component Diagram

{Text-based diagram using ASCII or mermaid syntax}

## Data Flow

{How data moves through the system}

## Key Design Decisions

{Important architectural choices and their rationale}

## Dependencies

### External Services
{APIs, databases, third-party services}

### Internal Dependencies
{Module dependency graph}
```

### Template: Module Documentation

```markdown
# {Module Name}

## Purpose

{What this module does and why it exists}

## Public API

### Functions/Methods
- `functionName(params)` - {brief description}

### Types/Interfaces
- `TypeName` - {what it represents}

## Usage Examples

{Code snippets showing how to use this module}

## Internal Architecture

{How the module is organized internally}

## Dependencies

- Uses: {other modules this depends on}
- Used by: {modules that depend on this}

## Key Files

| File | Purpose |
|------|---------|
| `main.ts` | Entry point |
| `types.ts` | Type definitions |
```

## Analysis Heuristics

### Prioritization Rules

1. **Start with entry points** - `main`, `index`, `app` files
2. **Follow the imports** - Trace from entry to dependencies
3. **Focus on public interfaces first** - Understand API before internals
4. **Skip generated code** - `*.min.js`, `*.d.ts`, `build/`, `dist/`
5. **Identify core vs peripheral** - Business logic over utilities

### Depth Guidelines

| Project Size | Module Doc Depth |
|-------------|------------------|
| Small (<10 files) | Document all files |
| Medium (10-50) | Focus on modules, summarize utilities |
| Large (50-200) | Top modules + architecture |
| Very Large (200+) | Architecture + key modules only |

### Pattern Recognition

**Signs of important modules:**
- Many inbound imports
- Located in `core/`, `lib/`, `services/`
- Contains business domain terminology
- Has its own tests

**Signs of utility/helper modules:**
- Generic names (`utils`, `helpers`, `common`)
- Few or no inbound imports
- Stateless functions

## Output Checklist

Before finalizing documentation, verify:

- [ ] Project purpose is clearly stated
- [ ] Entry points are identified
- [ ] Module purposes are documented
- [ ] Dependencies are mapped
- [ ] Key concepts are explained
- [ ] No internal jargon left unexplained
- [ ] Someone new can start navigating with this doc
