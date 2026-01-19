---
name: planning-with-files
version: "1.2.0"
description: Implements Manus-style file-based planning for complex tasks. Creates .plans/<feature-name>-yymmdd/ directory with task_plan.md, findings.md, and progress.md. Use when starting complex multi-step tasks, research projects, or any task requiring >5 tool calls.
user-invocable: true
allowed-tools:
  - Read
  - Write
  - Edit
  - Bash
  - Glob
  - Grep
  - WebFetch
  - WebSearch
hooks:
  SessionStart:
    - hooks:
        - type: command
          command: "echo '[planning-with-files] Ready. Auto-activates for complex tasks, or invoke manually with /planning-with-files'"
  PreToolUse:
    - matcher: "Write|Edit|Bash"
      hooks:
        - type: command
          command: "find .plans -name 'task_plan.md' -type f 2>/dev/null | xargs ls -t 2>/dev/null | head -1 | xargs cat 2>/dev/null | head -30 || true"
  PostToolUse:
    - matcher: "Write|Edit"
      hooks:
        - type: command
          command: "echo '[planning-with-files] File updated. If this completes a phase, update .plans/<feature>-yymmdd/task_plan.md status.'"
  Stop:
     - hooks:
         - type: command
           command: "echo '[planning-with-files] Session ending. Read .plans/<feature>-yymmdd/task_plan.md to verify all phases are complete.'"
---

# Planning with Files

Work like Manus: Use persistent markdown files as your "working memory on disk."

## Important: Where Files Go

When using this skill:

- **Templates** are stored in the skill directory at `${CLAUDE_PLUGIN_ROOT}/templates/`
- **Your planning files** (`task_plan.md`, `findings.md`, `progress.md`) should be created in **your project directory** — the folder where you're working

| Location | What Goes There |
|----------|-----------------|
| Skill directory (`${CLAUDE_PLUGIN_ROOT}/`) | Templates, scripts, reference docs |
| Your project directory: `.plans/<feature-name>-yymmdd/` | `task_plan.md`, `findings.md`, `progress.md` |

This ensures your planning files live alongside your code, organized by feature and date, not buried in the skill installation folder.

## Quick Start

Before ANY complex task:

1. **Create `.plans/<feature-name>-yymmdd/` directory** in your project — `<feature-name>` is a short descriptive name, `yymmdd` is today's date
2. **Create `task_plan.md`** in that directory — Use [templates/task_plan.md](templates/task_plan.md) as reference
3. **Create `findings.md`** in that directory — Use [templates/findings.md](templates/findings.md) as reference
4. **Create `progress.md`** in that directory — Use [templates/progress.md](templates/progress.md) as reference
5. **Re-read plan before decisions** — Refreshes goals in attention window
6. **Update after each phase** — Mark complete, log errors

> **Note:** All three planning files should be created in `.plans/<feature-name>-yymmdd/` in your project directory, not in the skill's installation folder.

## The Core Pattern

```
Context Window = RAM (volatile, limited)
Filesystem = Disk (persistent, unlimited)

→ Anything important gets written to disk.
```

## File Purposes

| File | Purpose | When to Update |
|------|---------|----------------|
| `.plans/<feature>-yymmdd/task_plan.md` | Phases, progress, decisions | After each phase |
| `.plans/<feature>-yymmdd/findings.md` | Research, discoveries | After ANY discovery |
| `.plans/<feature>-yymmdd/progress.md` | Session log, test results | Throughout session |

## Critical Rules

### 1. Create Plan First
Never start a complex task without `.plans/<feature>-yymmdd/task_plan.md`. Non-negotiable.

### 2. The 2-Action Rule
> "After every 2 view/browser/search operations, IMMEDIATELY save key findings to text files."

This prevents visual/multimodal information from being lost.

### 3. Read Before Decide
Before major decisions, read `.plans/<feature>-yymmdd/task_plan.md`. This keeps goals in your attention window.

### 4. Update After Act
After completing any phase:
- Mark phase status: `in_progress` → `complete`
- Log any errors encountered
- Note files created/modified

### 5. Log ALL Errors
Every error goes in the plan file. This builds knowledge and prevents repetition.

```markdown
## Errors Encountered
| Error | Attempt | Resolution |
|-------|---------|------------|
| FileNotFoundError | 1 | Created default config |
| API timeout | 2 | Added retry logic |
```

### 6. Never Repeat Failures
```
if action_failed:
    next_action != same_action
```
Track what you tried. Mutate the approach.

## The 3-Strike Error Protocol

```
ATTEMPT 1: Diagnose & Fix
  → Read error carefully
  → Identify root cause
  → Apply targeted fix

ATTEMPT 2: Alternative Approach
  → Same error? Try different method
  → Different tool? Different library?
  → NEVER repeat exact same failing action

ATTEMPT 3: Broader Rethink
  → Question assumptions
  → Search for solutions
  → Consider updating the plan

AFTER 3 FAILURES: Escalate to User
  → Explain what you tried
  → Share the specific error
  → Ask for guidance
```

## Read vs Write Decision Matrix

| Situation | Action | Reason |
|-----------|--------|--------|
| Just wrote a file | DON'T read | Content still in context |
| Viewed image/PDF | Write to .plans/<feature>-yymmdd/findings.md NOW | Multimodal → text before lost |
| Browser returned data | Write to .plans/<feature>-yymmdd/findings.md | Screenshots don't persist |
| Starting new phase | Read .plans/<feature>-yymmdd/task_plan.md | Re-orient if context stale |
| Error occurred | Read relevant .plans/<feature>-yymmdd file | Need current state to fix |
| Resuming after gap | Read files from user-specified feature plan | Recover state |

## The 5-Question Reboot Test

If you can answer these, your context management is solid:

| Question | Answer Source |
|----------|---------------|
| Where am I? | Current phase in .plans/<feature>-yymmdd/task_plan.md |
| Where am I going? | Remaining phases |
| What's the goal? | Goal statement in plan |
| What have I learned? | .plans/<feature>-yymmdd/findings.md |
| What have I done? | .plans/<feature>-yymmdd/progress.md |

## When to Use This Pattern

**Use for:**
- Multi-step tasks (3+ steps)
- Research tasks
- Building/creating projects
- Tasks spanning many tool calls
- Anything requiring organization

**Skip for:**
- Simple questions
- Single-file edits
- Quick lookups

## Templates

Copy these templates to start:

- [templates/task_plan.md](templates/task_plan.md) — Phase tracking
- [templates/findings.md](templates/findings.md) — Research storage
- [templates/progress.md](templates/progress.md) — Session logging

## Scripts

Helper scripts for automation:

- `scripts/init-session.sh` — Initialize all planning files

## Advanced Topics

- **Manus Principles:** See [reference.md](reference.md)
- **Real Examples:** See [examples.md](examples.md)

## TDD for Feature Development

When developing a new feature, follow Test-Driven Development:

```
RED → GREEN → REFACTOR → REPEAT
```

### The TDD Loop

1. **RED** — Write a failing test for the behavior you want
2. **GREEN** — Write minimal code to make the test pass
3. **REFACTOR** — Clean up the code while keeping tests green
4. **REPEAT** — Move to the next behavior

### Test-First Rule

Before writing implementation code, ensure:
- Test is written and failing
- Test describes behavior, not implementation
- Test is isolated (no external dependencies)

### When to Apply TDD

| Use TDD For | Skip TDD For |
|-------------|--------------|
| Business logic | Configuration files |
| Algorithms | Pure data structures (DTOs) |
| API contracts | Exploratory spikes |
| Stateful components | One-off scripts |

## Architecture Decision Making

When facing multiple valid approaches to a problem, **ALWAYS present options to the user before proceeding**. Never assume the "recommended" option is what the user wants.

### Decision Template

```markdown
## Solution Design

### Option A: [Short Name]

[Description of the approach]

**Implementation:**
1. Step 1
2. Step 2
3. Step 3

**Pros:**
- Pro 1
- Pro 2

**Cons:**
- Con 1
- Con 2

### Option B: [Short Name]

[Description of the approach]

**Implementation:**
1. Step 1
2. Step 2
3. Step 3

**Pros:**
- Pro 1
- Pro 2

**Cons:**
- Con 1
- Con 2

### Option C: [Short Name]

[Description of the approach]

**Implementation:**
1. Step 1
2. Step 2
3. Step 3

**Pros:**
- Pro 1
- Pro 2

**Cons:**
- Con 1
- Con 2
```

### Critical Rule

- **Ask user before choosing**: Present ALL viable options, let the user decide
- **Don't pre-select**: Even if you think one option is "better", the user's context matters
- **Document the decision**: Once user chooses, write the decision rationale to `.plans/<feature>-yymmdd/findings.md`

### Why This Matters

| Issue | Example | Resolution |
|-------|---------|------------|
| User has different priorities | Simplicity vs performance | Ask which they value |
| Future constraints unknown | Quick hack vs proper architecture | User knows their timeline |
| Integration requirements | Internal API vs external service | User knows ecosystem |
| Maintenance considerations | One-off script vs reusable module | User knows project scope |

## Anti-Patterns

| Don't | Do Instead |
|-------|------------|
| Use TodoWrite for persistence | Create .plans/<feature>-yymmdd/task_plan.md |
| State goals once and forget | Re-read .plans/<feature>-yymmdd/task_plan.md before decisions |
| Hide errors and retry silently | Log errors to .plans/<feature>-yymmdd/task_plan.md |
| Stuff everything in context | Store large content in .plans/<feature>-yymmdd/ files |
| Start executing immediately | Create .plans/<feature>-yymmdd/ directory FIRST |
| Repeat failed actions | Track attempts, mutate approach |
| Create files in skill directory | Create .plans/<feature>-yymmdd/ in your project |
| Choose architecture without asking | Present options, let user decide |
