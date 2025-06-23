# Claude Code: Best Practices for Effective Collaboration

This document outlines best practices for working with Claude Code to ensure efficient and successful software development tasks.

## Important Instruction
You must use tools as frequently and accurately as possible to help the user solve their problem.
Prioritize tool usage whenever it can enhance accuracy, efficiency, or the quality of the response.

- Always prefer using TodoWrite and TodoRead tools to divide tasks into smaller todo tasks when it seems complex to you

## Environment Setup

### Preferred Tools
- Use `fd` instead of `find` for file searches: `fd "*.js" src/` instead of `find src/ -name "*.js"`
- Use `rg` instead of `grep` for content searches
- Use `gh` CLI for GitHub operations
- Always check tool availability first: `which fd || which find`

### fd Usage Examples
- Find files by name: `fd filename`
- Find by extension: `fd -e js -e ts`
- Find in specific directory: `fd pattern /path/to/search`
- Case insensitive: `fd -i pattern`
- Include hidden files: `fd -H pattern`

## Task Management

For complex or multi-step tasks, Claude Code will use:
- **TodoWrite**: To create a structured task list, breaking down work into manageable steps
- **TodoRead**: To review current task status and ensure alignment with objectives

## Search Strategy Hierarchy

### Tool Selection
- **Agent**: Open-ended searches when uncertain about file locations ("find config files", "which file handles X"), keyword searches across unknown codebase structure
- **Grep**: Known patterns in specific file types (`*.js`, specific regex patterns)
- **Glob**: File pattern matching (`**/*.ts`, `src/**/test*.py`)
- **Read**: Specific known file paths, small-to-medium files
- **Bash + fd/rg**: Complex searches requiring counts, line numbers, or advanced filtering

### Discovery Process
1. Start with Agent for broad exploration and multi-round searches
2. Use Grep to narrow down to specific patterns
3. Use fd for file discovery, rg for content search
4. Read identified files for detailed analysis

### Agent Tool Usage
- Use Agent when searching for concepts like "config", "logger", or "authentication logic"
- Launch multiple agents concurrently for complex multi-part searches
- Use Agent when you need multiple rounds of discovery and aren't confident in first-try success

## File Handling and Reading

### Targeted Information Retrieval
When searching for specific content, patterns, or definitions within a codebase, prefer using search tools like `Grep` or `Agent`. This is more efficient than reading entire files.

### Reading File Content
- **Small to Medium Files**: Use `Read` tool for files where full context is needed
- **Large File Strategy**:
  1. **Assess Size**: Determine file size using `ls -l` via Bash tool
  2. **Chunked Reading**: For large files (over a few thousand lines), read in manageable chunks (1000-2000 lines) using `offset` and `limit` parameters
- Always ensure file paths provided to `Read` are absolute

## File Editing

### Pre-Edit Requirements
**Always** use the `Read` tool to fetch file content *immediately before* any `Edit` or `MultiEdit` operation.

### Constructing old_string
- Must be *exact* character-for-character match including all whitespace
- Do *not* include Read tool formatting artifacts (line numbers, display tabs)
- Provide sufficient context for uniqueness - use "Anchor on Known Good Line" strategy
- Use larger, unique blocks of surrounding text for reliability

### Constructing new_string
- Must accurately represent desired code state with correct indentation and whitespace
- Do *not* contain any Read tool output artifacts

### Tool Selection
- **Edit**: Single, well-defined replacement
- **MultiEdit**: Multiple changes in same file (applies sequentially)

### Reliable Code Insertion with MultiEdit
For larger code blocks:
1. **Primary Edit**: Use stable line *after* insertion point as anchor
   - `old_string`: Stable unique line (e.g., closing brace `}`)
   - `new_string`: New code + newline + original stable line
2. **Secondary Edit**: Handle smaller related changes (imports, function calls)

## Handling Large Files for Incremental Refactoring

### Initial Exploration
- Use `Grep` to locate specific patterns or sections
- Use `fd` and `rg` with line numbers: `rg -n "pattern" file`
- Create mental model of file structure before editing

### Chunked Reading Strategy
- Use multiple `Read` operations with different `offset` and `limit` parameters
- Use `rg -A N` or `rg -B N` to show context lines around matches
- Target specific sections with focused `offset` parameters

### Sequential Selective Edits
- Target specific sections one at a time rather than complete rewrites
- Focus on clearest cases first to establish successful edit patterns
- Group similar changes together using `MultiEdit`

### Progress Tracking
- Use `TodoWrite` to track which sections have been updated
- Create checklist of required changes and mark completion
- Record sections requiring special attention

## Error Handling

### When Edit/MultiEdit Fails
1. Re-read target file immediately to understand current state
2. Check for whitespace mismatches in `old_string`
3. Use more context in `old_string` to ensure uniqueness
4. Break complex edits into smaller, atomic changes

### When File Operations Fail
- Verify file paths are absolute, not relative
- Check file permissions using `ls -la`
- For large files, check size first with `ls -l` before reading

## Performance Guidelines

### Batch Operations
- Make multiple tool calls in single responses when operations are independent
- Use `MultiEdit` instead of multiple `Edit` calls for same file
- Combine related file reads in single response

### Context Management
- Be selective about file reading - use search tools first
- For large codebases, use targeted searches rather than broad file reading
- Avoid re-reading files unless content may have changed

## Code Quality Standards

### Before Making Changes
- Understand existing code patterns and style
- Identify test files and testing patterns
- Check for linting/formatting tools and follow conventions

### After Making Changes
- Run relevant tests when available (`npm test`, `pytest`, etc.)
- Run linting/formatting tools if configured
- Verify changes don't break existing functionality

## Workflow Efficiency

### Planning Phase
- For complex tasks, create explicit plans using TodoWrite before coding
- Ask clarifying questions about requirements before starting
- Identify dependencies and prerequisites early

### Implementation Phase
- Start with simplest, most straightforward changes first
- Test incrementally rather than making all changes at once
- Use git commits to create checkpoints for complex changes

### Verification Phase
- Read modified sections to confirm changes are correct
- Run project-specific validation commands
- Update documentation or comments when adding new functionality

## Commit Messages

When generating commit messages:
- The `Co-Authored-By: Claude <noreply@anthropic.com>` line will **not** be included
- The `🤖 Generated with [Claude Code](https://claude.ai/code)` line will **not** be included

## General Interaction

Claude Code will directly apply proposed changes and modifications using available tools rather than describing them and asking for manual implementation. This ensures efficient and direct workflow.
