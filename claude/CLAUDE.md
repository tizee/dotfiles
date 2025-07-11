# Claude Code: Best Practices for Effective Collaboration

This document outlines best practices for working with Claude Code to ensure efficient and successful software development tasks.

## Important Instruction
You must use tools as frequently and accurately as possible to help the user solve their problem.
Prioritize tool usage whenever it can enhance accuracy, efficiency, or the quality of the response.

- Always prefer using TodoWrite and TodoRead tools to divide tasks into smaller todo tasks when it seems complex to you

## Task Completion and Reporting

### Mandatory Response Requirement
**For ANY task (regardless of complexity), you must provide a summary response.** This ensures users receive acknowledgment and confirmation of completed work.

- **Simple Tasks**: Provide a one-sentence summary confirming completion
- **Complex Tasks**: Provide a comprehensive completion summary with detailed breakdown

### Response Guidelines
Never leave tasks without feedback. Always respond with:
- Confirmation that the task was completed
- Brief description of what was accomplished
- Any relevant outcomes or results

When completing complex or multi-step tasks, provide a comprehensive completion summary that includes:

### Summary Structure
1. **Task Overview**: Briefly restate what was requested
2. **Work Completed**: List major accomplishments with specific details
   - Use bullet points for clear organization
   - Include quantifiable results when applicable (e.g., "converted 116 assert statements", "organized into 5 test classes")
   - Reference specific files, functions, or line numbers that were modified
3. **Key Changes**: Highlight the most important modifications made
4. **Verification**: Confirm that the solution works (e.g., "tests pass", "functionality verified")
5. **Final State**: Describe the improved state of the codebase

### When to Provide Detailed Summaries
- Multi-step refactoring tasks
- Large-scale code organization or restructuring
- Complex feature implementations
- Bug fixes that required significant investigation
- Tasks that involved multiple files or systems

### Summary Style Guidelines
- Be thorough but concise - aim for clarity over brevity
- Use active voice and specific technical language
- Include section headers (## Work Summary:) for major summaries
- Emphasize the value and impact of the changes made
- Show pride in thorough, well-executed work

This approach demonstrates professionalism, attention to detail, and ensures the user understands exactly what was accomplished.

## Environment Setup

### Preferred Tools
- **Primary**: Use `sg` (ast-grep) for structural code searches and syntax-aware analysis
- **MANDATORY**: Use `fd` instead of `find` for file searches: `fd "*.js" src/` instead of `find src/ -name "*.js"`
- **MANDATORY**: Use `rg` instead of `grep` for plain-text content searches (only when ast-grep isn't applicable)
- Use `gh` CLI for GitHub operations
- **Note**: ast-grep is pre-installed and available, just like ripgrep (`rg`) and fd - no availability checks needed

### Tool Priority Enforcement
**CRITICAL**: You MUST use modern tools over legacy alternatives:
- ✅ **fd** → ❌ find
- ✅ **rg** → ❌ grep
- ✅ **sg** → ❌ regex-based searches for code

**Never use `find` or `grep` commands directly.** Always use `fd` and `rg` instead.

### rg Usage Examples

Here's the revised **ripgrep (rg) Cheat Sheet** in the requested format:

#### **Basic Search**
- Search recursively: `rg "pattern"`
- Search in directory: `rg "pattern" path/`
- Case-insensitive: `rg -i "pattern"`
- Show line numbers: `rg -n "pattern"`
- Whole-word match: `rg -w "pattern"`
- Literal string: `rg -F "$path"`

#### **File Types**
- Python files only: `rg --type=py "import"`
- Multiple file types: `rg --type=js,ts "console"`
- Exclude file types: `rg -Tjson "data"`

#### **Context Control**
- Show 3 lines after: `rg -A 3 "error"`
- Show 2 lines before: `rg -B 2 "warning"`
- Show 5 lines around: `rg -C 5 "panic"`

#### **Multiline Search**
- Basic multiline: `rg -U "start(.|\n)*?end"`
- ⚠️ **Warning**: Cannot combine `-U` with `-A`/`-B`/`-C`

#### **Output Formats**
- Filenames only: `rg -l "config"`
- Match counts: `rg -c "test"`
- Only matched text: `rg -o "user_\w+"`
- JSON output: `rg --json "pattern"`

#### **Advanced**
- Multiple patterns: `rg -e "err" -e "warn"`
- PCRE2 regex: `rg --pcre2 "(?<=api_)v\d+"`
- Search compressed: `rg -z "password" file.gz`

#### **Performance**
- Ignore .gitignore: `rg --no-ignore`
- Search everything: `rg -uuu`
- Limit depth: `rg --max-depth 3 "x"`

#### **Pro Tips**
- Find hidden files: `rg --hidden ".*env"`
- Exclude minified: `rg -g '!*.min.js' "x"`

### fd Usage Examples

Here's the comprehensive **fd (file finder) Cheat Sheet** - an alternative to `find` that aims to be faster and easier to use:

#### **Basic Search**
- Recursively find files matching pattern: `fd "string|regex"`
- Find files that begin with pattern: `fd "^foo"`
- Find files by exact name: `fd filename`
- Case insensitive search: `fd -i pattern`

#### **File Extensions**
- Find by single extension: `fd --extension txt`
- Find by multiple extensions: `fd -e js -e ts`
- Alternative syntax: `fd -e js,ts`

#### **Directory Control**
- Find in specific directory: `fd "pattern" path/to/directory`
- Limit search depth: `fd --max-depth 3 pattern`
- Search only directories: `fd --type d pattern`
- Search only files: `fd --type f pattern`

#### **Visibility Control**
- Include hidden files: `fd --hidden pattern`
- Include ignored files: `fd --no-ignore pattern`
- Include both hidden and ignored: `fd -H --no-ignore pattern`
- Show absolute paths: `fd --absolute-path pattern`

#### **Output Control**
- Execute command on results: `fd pattern -x command`
- Print null-separated: `fd -0 pattern`
- Show file details: `fd -l pattern`
- Follow symlinks: `fd -L pattern`

#### **Advanced Usage**
- Multiple patterns: `fd -e js -e ts "test|spec"`
- Exclude patterns: `fd --exclude node_modules pattern`
- Full path search: `fd --full-path "/src.*test"`
- Regex mode: `fd --regex "\.(js|ts)$"`

#### **Performance Tips**
- Use specific extensions when possible: `fd -e py` vs `fd "*.py"`
- Combine with other tools: `fd -e js | xargs grep "pattern"`
- Use parallel execution: `fd pattern -x parallel_command`

#### **Common Use Cases**
- Find config files: `fd "config" --type f`
- Find test files: `fd "test" -e js -e ts`
- Find all JavaScript: `fd -e js -e jsx -e ts -e tsx`
- Find recently modified: `fd pattern -x ls -la`
- Clean build artifacts: `fd "dist|build" --type d -x rm -rf`

### Basic ast-grep Usage Examples
- Find function calls: `sg -p 'functionName($$)' -l javascript .`
- Find class definitions: `sg -p 'class $NAME { $$ }' -l typescript .`
- Find imports: `sg -p 'import { $$ } from "$MODULE"' -l javascript .`
- Interactive rewrite: `sg -p '$OLD_PATTERN' --rewrite '$NEW_PATTERN' -l python --interactive .`

## Task Management

For complex or multi-step tasks, Claude Code will use:
- **TodoWrite**: To create a structured task list, breaking down work into manageable steps
- **TodoRead**: To review current task status and ensure alignment with objectives

## Search Strategy Hierarchy

### Tool Selection Priority
1. **ast-grep (sg)**: **PRIMARY** - Structural code patterns, syntax-aware searches, language-specific refactoring
2. **Agent**: Broad exploration when ast-grep isn't applicable, semantic understanding, multi-round discovery
3. **fd**: **MANDATORY** for file discovery - Never use `find`
4. **rg**: **MANDATORY** for plain-text patterns - Never use `grep`
5. **Glob**: File pattern matching (`**/*.ts`, `src/**/test*.py`)
6. **Read**: Specific known file paths, small-to-medium files
7. **Bash**: For executing fd/rg/sg commands and complex searches requiring counts, line numbers, or advanced filtering

**Note**: All command-line tools (fd, rg, sg) are executed through the Bash tool. The Bash tool is the interface for running these modern alternatives.

### File Discovery Strategy
**Always use `fd` for file discovery tasks:**
- ✅ `fd "*.js" src/` → ❌ `find src/ -name "*.js"`
- ✅ `fd -e py test/` → ❌ `find test/ -name "*.py"`
- ✅ `fd config --type f` → ❌ `find . -name "*config*" -type f`

### Discovery Process
1. **Use ast-grep first** for any code structure analysis:
   - Function definitions, calls, or usage patterns
   - Class definitions, inheritance, or method patterns
   - Import/require statements or module usage
   - Variable assignments, declarations, or references
   - Language-specific syntax patterns
2. **Fall back to Agent** for broad semantic exploration when ast-grep cannot handle the query
3. **Use rg only** for plain-text searches when ast-grep isn't applicable
4. **Use fd** for file discovery based on names/paths - **NEVER use find**
5. **Read** identified files for detailed analysis

### ast-grep First Strategy
**ALWAYS prioritize ast-grep when the search involves:**
- Code structure or syntax (functions, classes, methods, etc.)
- Language-aware pattern matching (not just text matching)
- Refactoring or code transformation needs
- Finding usage patterns across codebases
- Cross-language searches in polyglot repositories

**Example Decision Matrix:**
- ❌ "Find all TODO comments" → Use rg (plain text)
- ✅ "Find all function calls to `fetchData`" → Use ast-grep
- ✅ "Find all React hooks usage" → Use ast-grep
- ✅ "Find all class definitions extending BaseClass" → Use ast-grep
- ❌ "Find files containing string 'password'" → Use rg (plain text)
- ✅ "Find all import statements from specific module" → Use ast-grep
- ❌ "Find all .env files" → Use fd
- ❌ "Find Python test files" → Use fd

### Agent Tool Usage
- Use Agent when ast-grep cannot handle the semantic complexity
- Use Agent for conceptual searches like "authentication logic" or "configuration handling"
- Launch multiple agents concurrently for complex multi-part searches
- Use Agent when you need multiple rounds of discovery and aren't confident in first-try success

## AST-Grep Integration Protocol

### When to Use ast-grep

Use `sg` (ast-grep) instead of plain regex or text search when:

- **Structural code patterns** are involved (e.g., finding all function calls, class definitions, or method implementations)
- **Language-aware refactoring** is required (e.g., renaming variables, updating function signatures, or changing imports)
- **Complex code analysis** is needed (e.g., finding all usages of a pattern across different syntactic contexts)
- **Cross-language searches** are necessary (e.g., working with both JavaScript and TypeScript in a monorepo)
- **Semantic code understanding** is important (e.g., finding patterns based on code structure, not just text)

### Command Templates

#### Basic Search Pattern:
```bash
sg -p '$PATTERN' -l $LANGUAGE $PATH
```

#### Search with Context:
```bash
sg -p '$PATTERN' -l $LANGUAGE -C 3 $PATH  # Show 3 lines of context
```

#### Interactive Rewrite:
```bash
sg -p '$OLD_PATTERN' --rewrite '$NEW_PATTERN' -l $LANGUAGE --interactive $PATH
```

#### JSON Output for Processing:
```bash
sg -p '$PATTERN' -l $LANGUAGE --json $PATH
```

### Common Use Cases and Patterns

#### JavaScript/TypeScript Patterns:
- **Find function calls:**
  `sg -p 'functionName($$)' -l javascript .`
- **Find class definitions:**
  `sg -p 'class $NAME { $$ }' -l typescript .`
- **Find method calls on objects:**
  `sg -p '$OBJ.$METHOD($$)' -l typescript .`
- **Find import statements:**
  `sg -p 'import { $$ } from "$MODULE"' -l javascript .`
- **Find React hooks:**
  `sg -p 'const [$STATE, $SETTER] = useState($$)' -l typescript .`
- **Find async functions:**
  `sg -p 'async function $NAME($$) { $$ }' -l javascript .`

#### Python Patterns:
- **Find function definitions:**
  `sg -p 'def $NAME($$): $$' -l python .`
- **Find class definitions:**
  `sg -p 'class $NAME: $$' -l python .`
- **Find method calls:**
  `sg -p '$OBJ.$METHOD($$)' -l python .`
- **Find imports:**
  `sg -p 'from $MODULE import $$' -l python .`
- **Find decorators:**
  `sg -p '@$DECORATOR' -l python .`

#### Ruby Patterns:
- **Find class definitions:**
  `sg -p 'class $NAME < $$; $$; end' -l ruby .`
- **Find method definitions:**
  `sg -p 'def $NAME($$); $$; end' -l ruby .`
- **Find method calls:**
  `sg -p '$OBJ.$METHOD($$)' -l ruby .`

#### General Patterns:
- **Find variable assignments:**
  `sg -p '$VAR = $$' -l $LANG .`
- **Find conditional statements:**
  `sg -p 'if $CONDITION { $$ }' -l $LANG .`

### Pattern Syntax Reference

- **`$VAR`** — matches any single node and captures it
- **`$$`** — matches zero or more nodes (wildcard)
- **`$$$`** — matches one or more nodes
- **Literal code** — matches exactly as written
- **Indentation insensitive** — matches regardless of whitespace/formatting

### Supported Languages

- **Web**: `javascript`, `typescript`, `html`, `css`
- **Backend**: `python`, `ruby`, `go`, `rust`, `java`, `c`, `cpp`
- **Config**: `yaml`, `json`, `toml`
- **And many more** - see [full list](https://ast-grep.github.io/reference/languages.html)

### Integration Workflow

#### Using ast-grep workflow:
1. **Identify** if the task involves structural code patterns or language-aware refactoring
2. **Determine** the appropriate language(s) to search
3. **Construct** the pattern using ast-grep syntax
4. **Run** ast-grep to gather precise structural information
5. **Use** results to inform code edits, refactoring, or further analysis

#### Example Workflow

When asked to "find all service objects that call `perform` method":

1. **Run ast-grep search:**
   ```bash
   sg -p 'perform($$)' -l ruby app/services/
   ```
2. **Analyze** results structurally
3. **Use** Agent or Read tools for additional context if needed
4. **Make** informed edits based on structural understanding

### Advanced Usage

#### Rewrite Patterns:
```bash
# Simple rewrite
sg -p '$PROP && $PROP()' --rewrite '$PROP?.()' -l typescript

# Interactive rewrite session
sg -p '$OLD' --rewrite '$NEW' -l python --interactive

# Auto-apply all changes
sg -p '$OLD' --rewrite '$NEW' -l javascript --update-all
```

#### Complex Patterns:
```bash
# Find nested patterns
sg -p 'if ($COND) { if ($INNER) { $$ } }' -l javascript

# Find patterns with specific context
sg -p 'class $NAME { constructor($$) { $$ } }' -l typescript
```

### Key Benefits Over Regex

1. **Language-aware** — understands syntax and semantics
2. **Structural matching** — finds patterns regardless of formatting
3. **Cross-language** — works consistently across different languages
4. **Precise refactoring** — makes structural changes safely
5. **Context-aware** — understands code hierarchy and scope

**Always prefer ast-grep for code structure analysis over regex-based approaches.**

## File Handling and Reading

### Targeted Information Retrieval
When searching for specific content, patterns, or definitions within a codebase, prefer using search tools in this order:
1. **ast-grep (`sg`)** for structural code patterns and syntax-aware searches
2. **Agent** for semantic understanding and complex multi-round discovery
3. **fd** for file discovery and name-based searches - **NEVER use find**
4. **rg** for plain-text patterns only when structural search isn't applicable - **NEVER use grep**

This is more efficient than reading entire files and provides better accuracy for code analysis.

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

### Edit Operation Best Practices

**Principle: Default to Granular Edits.** Your default behavior must be to break down any non-trivial modification into the smallest possible, sequential `Edit` or `MultiEdit` calls. Avoid attempting large, complex changes in a single operation. This is not a suggestion, but a requirement to reduce errors and provide a better user experience.

To ensure reliable and efficient file editing:

#### Size Limits for Edit Operations
- **Single Edit**: Limit `old_string` and `new_string` to 50-100 lines maximum
- **MultiEdit**: Use at most 3-5 edit operations per call
- **Large Changes**: Break into multiple sequential tool calls instead of one massive operation

#### Breaking Down Large Modifications
When facing large file modifications:
1. **Identify Logical Sections**: Break changes into independent, logically cohesive chunks
2. **Sequential Approach**: Use multiple separate Edit/MultiEdit calls in sequence
3. **Incremental Progress**: Complete one section before moving to the next
4. **Use TodoWrite**: Track progress across multiple edit operations

#### Safe MultiEdit Practices
- **Limit Scope**: Each edit within MultiEdit should target small, specific sections
- **Avoid Overlapping**: Ensure edits don't interfere with each other
- **Test Incrementally**: Verify each change works before proceeding

#### Example: Large File Refactoring Strategy
Instead of:
```
// DON'T: Single massive MultiEdit with 10+ operations
MultiEdit([edit1, edit2, edit3, ... edit15])
```

Use:
```
// DO: Multiple smaller operations
MultiEdit([edit1, edit2, edit3])  // First logical group
MultiEdit([edit4, edit5, edit6])  // Second logical group
Edit(edit7)                       // Single targeted change
```

### Reliable Code Insertion with MultiEdit
For larger code blocks:
1. **Primary Edit**: Use stable line *after* insertion point as anchor
   - `old_string`: Stable unique line (e.g., closing brace `}`)
   - `new_string`: New code + newline + original stable line
2. **Secondary Edit**: Handle smaller related changes (imports, function calls)

## Handling Large Files for Incremental Refactoring

### Initial Exploration
- **Primary**: Use `sg` (ast-grep) to locate structural code patterns: `sg -p 'pattern' -l language -C 3 file`
- **MANDATORY**: Use `fd` for file discovery: `fd pattern /path` - **NEVER use find**
- **Fallback**: Use `rg` with line numbers for plain-text patterns: `rg -n "pattern" file` - **NEVER use grep**
- Create mental model of file structure before editing

### Chunked Reading Strategy
- Use multiple `Read` operations with different `offset` and `limit` parameters
- **Preferred**: Use `sg -A N` or `sg -B N` to show context lines around structural matches
- **Fallback**: Use `rg -A N` or `rg -B N` for plain-text context
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
5. **Self-Correction**: If an edit fails, you MUST assume your approach was too broad. Your next step is to immediately break the failed edit into smaller, more granular `Edit` or `MultiEdit` calls and retry. Do not attempt the same large edit again.

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
