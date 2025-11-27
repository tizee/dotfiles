# Claude


## Claude Code

### Quick Setup with Makefile

The easiest way to set up all Claude Code configurations is using the included Makefile:

```bash
# Link all configurations at once
make install

# Or link individual components
make link-settings
make link-commands
make link-skills
make link-agents

# Check status of all symlinks
make check

# Remove all symlinks
make clean

# Show all available commands
make help
```

### Manual Setup

If you prefer to link configurations manually:

**Settings:**
```bash
ln -sv ~/.config/claude/claude-code-settings.json ~/.claude/settings.json
```

**Commands:**

See Claude Code's doc on [Personal commands](https://docs.anthropic.com/en/docs/claude-code/slash-commands#personal-commands)

```bash
ln -sv ~/.config/claude/commands ~/.claude/commands
```

**Skills:**
```bash
ln -sv ~/.config/claude/skills ~/.claude/skills
```

**Agents:**

See Claude Code's doc on [Subagents](https://docs.anthropic.com/en/docs/claude-code/subagents)

```bash
ln -sv ~/.config/claude/agents ~/.claude/agents
```

### CLAUDE.md

Here is my user memory config for Claude Code that would be linked to `~/.claude/CLAUDE.md`.

It includes personal preferences for all projects	Code styling preferences, personal tooling shortcuts.

see Claude Code's doc on [memory](https://docs.anthropic.com/en/docs/claude-code/memory)

References:

- [Leaked Claude Code system prompts](https://gist.github.com/wong2/e0f34aac66caf890a332f7b6f9e2ba8f)
- [Official Claude Code Best practices](https://www.anthropic.com/engineering/claude-code-best-practices)

## Available Tools

Claude Code provides a comprehensive set of tools for software development tasks. These tools enable file operations, code search, command execution, web access, and task management.

### File Operations

#### Read
Reads files from the local filesystem with support for large files.
- Supports absolute file paths
- Can read in chunks using `offset` and `limit` parameters
- Handles images, PDFs, and Jupyter notebooks
- Returns content with line numbers

**Usage:**
```
Read file_path="/absolute/path/to/file"
Read file_path="/path/to/largefile" offset=1000 limit=500
```

#### Edit
Performs exact string replacements in files for precise code modifications.
- Requires exact character-for-character match for `old_string`
- Supports `replace_all` flag for renaming across the file
- Must read file before editing
- Best for single, well-defined replacements

**Usage:**
```
Edit file_path="/path/to/file" old_string="exact match" new_string="replacement"
Edit file_path="/path/to/file" old_string="oldName" new_string="newName" replace_all=true
```

#### Write
Writes new files or overwrites existing files.
- Must read existing files before overwriting
- Use absolute paths, not relative
- Prefer Edit over Write for existing files

**Usage:**
```
Write file_path="/absolute/path/to/file" content="file contents"
```

#### NotebookEdit
Edits Jupyter notebook (.ipynb) cells.
- Supports replace, insert, and delete modes
- Can target cells by ID or index
- Handles code and markdown cells

**Usage:**
```
NotebookEdit notebook_path="/path/to/notebook.ipynb" cell_id="abc123" new_source="print('hello')"
NotebookEdit notebook_path="/path/to/notebook.ipynb" edit_mode="insert" cell_type="code" new_source="new cell"
```

### Search & Discovery

#### Glob
Fast file pattern matching for finding files by name patterns.
- Supports glob patterns like `**/*.js`, `src/**/*.ts`
- Returns sorted by modification time
- Works with any codebase size

**Usage:**
```
Glob pattern="**/*.js"
Glob pattern="src/**/*.py" path="/specific/directory"
```

#### Grep
Powerful search tool built on ripgrep for content searching.
- Supports full regex syntax
- Multiple output modes: content, files_with_matches, count
- Filter by file type or glob pattern
- Context lines with -A, -B, -C flags
- Multiline mode support

**Usage:**
```
Grep pattern="functionName" output_mode="files_with_matches"
Grep pattern="TODO" output_mode="content" -n=true -C=3
Grep pattern="import.*React" type="js" output_mode="content"
```

#### Bash
Execute bash commands for terminal operations.
- Use for git, npm, docker, etc.
- Can run commands in background
- Supports timeout configuration
- **Use specialized tools (fd, rg, sg) instead of find/grep/cat when possible**

**Usage:**
```
Bash command="git status"
Bash command="npm test" timeout=120000
Bash command="long-running-process" run_in_background=true
```

**Preferred command-line tools (via Bash):**
- `fd` instead of `find` for file discovery
- `rg` instead of `grep` for text search
- `sg` (ast-grep) for structural code search

### Web Operations

#### WebFetch
Fetches and processes content from URLs.
- Converts HTML to markdown
- Processes content with AI
- 15-minute cache for repeated requests
- Read-only operation

**Usage:**
```
WebFetch url="https://example.com/docs" prompt="Summarize the main features"
```

#### WebSearch
Search the web for current information beyond Claude's knowledge cutoff.
- Supports domain filtering (allowed/blocked)
- Returns formatted search results with links
- Available in US only

**Usage:**
```
WebSearch query="latest TypeScript features 2025"
WebSearch query="React hooks" allowed_domains=["react.dev", "github.com"]
```

### Task Management

#### TodoWrite
Create and manage structured task lists for tracking progress.
- Break complex tasks into manageable steps
- Track task states: pending, in_progress, completed
- Limit to ONE task in_progress at a time
- Use for multi-step or complex tasks

**Usage:**
```
TodoWrite todos=[
  {content: "Task 1", activeForm: "Doing task 1", status: "completed"},
  {content: "Task 2", activeForm: "Doing task 2", status: "in_progress"},
  {content: "Task 3", activeForm: "Doing task 3", status: "pending"}
]
```

### Background Process Management

#### BashOutput
Retrieve output from running or completed background bash shells.
- Takes shell_id parameter
- Returns only new output since last check
- Supports regex filtering

**Usage:**
```
BashOutput bash_id="shell-123"
BashOutput bash_id="shell-123" filter="ERROR|WARN"
```

#### KillShell
Terminate a running background bash shell.

**Usage:**
```
KillShell shell_id="shell-123"
```

### User Interaction

#### AskUserQuestion
Ask users questions during execution for clarification or decisions.
- Support 1-4 questions per call
- 2-4 options per question
- Supports multi-select mode
- Automatic "Other" option provided

**Usage:**
```
AskUserQuestion questions=[{
  question: "Which library should we use?",
  header: "Library",
  multiSelect: false,
  options: [
    {label: "React", description: "Popular UI library"},
    {label: "Vue", description: "Progressive framework"}
  ]
}]
```

### Planning & Workflow

#### ExitPlanMode
Exit plan mode after presenting an implementation plan.
- Only use for tasks requiring code implementation
- Not for research or exploration tasks
- Clarify ambiguities before exiting plan mode

**Usage:**
```
ExitPlanMode plan="1. Create component\n2. Add tests\n3. Update docs"
```

### Extensions

#### Skill
Execute specialized skills within the conversation.
- Skills provide domain-specific capabilities
- Available skills shown in `<available_skills>` section
- Example: tmux skill for remote control of tmux sessions

**Usage:**
```
Skill skill="tmux"
```

#### SlashCommand
Execute custom slash commands defined in `.claude/commands/`.
- Commands expand to prompts from markdown files
- Only use for custom commands, not built-in CLI commands

**Usage:**
```
SlashCommand command="/review-pr 123"
```

## Tool Usage Best Practices

### Prefer Specialized Tools
- Use Read/Edit/Write for file operations, not cat/sed/echo via Bash
- Use Glob for file patterns, not find
- Use Grep for content search, not grep
- Use ast-grep (sg) for structural code search

### Parallel Tool Calls
When operations are independent, call multiple tools in parallel within a single response for better performance.

### Tool Selection Hierarchy
1. **ast-grep (sg)** - Structural code patterns
2. **Agent/Task** - Complex exploration
3. **fd** - File discovery
4. **rg** - Text search
5. **Glob** - Pattern matching
6. **Read** - Known file paths

## desktop app config

```
vim "~/Library/Application Support/Claude/claude_desktop_config.json"
```

### Debugging

- MacOS

```
open ~/Library/Logs/Claude
```
