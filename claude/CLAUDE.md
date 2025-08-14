# Claude Code: Best Practices for Effective Collaboration

This document outlines best practices for working with Claude Code to ensure efficient and successful software development tasks.

## Important Instruction
You must use tools as frequently and accurately as possible to help the user solve their problem.
Prioritize tool usage whenever it can enhance accuracy, efficiency, or the quality of the response.

- Always prefer using TodoWrite and TodoRead tools to divide tasks into smaller todo tasks when it seems complex to you

## Strategic Software Design Principles

### Core Philosophy: Managing Complexity

**The primary challenge in software development is managing complexity.** Every design decision should evaluate whether it reduces or increases the overall complexity burden on developers.

### Deep Modules: Simple Interfaces, Powerful Functionality

Strive to create modules with simple interfaces that hide significant functionality:

**Deep Module Characteristics:**
- Simple, clean interface requiring minimal knowledge from users
- Complex implementation hidden behind the interface
- High functionality-to-interface-complexity ratio
- Users can accomplish significant work with minimal interface calls

**Create Deep Modules Instead:**
- Design interfaces that provide substantial functionality with minimal complexity
- Encapsulate implementation details behind clean abstractions
- Enable users to accomplish tasks without understanding internal concepts

### Strategic vs Tactical Programming

**Strategic Programming:** Invest time in good design to create maintainable systems.
- Primary goal: produce excellent design that also works
- Invest 10-20% additional time in design considerations
- Focus on long-term maintainability over immediate completion
- Continuously refactor to improve design quality

**Choose Strategic Programming:** Invest in sustainable design for long-term success.
- Balance immediate functionality with excellent design
- Continuously refactor to maintain code quality
- Build systems that become easier to modify over time

### Information Hiding

Modules should encapsulate design decisions and implementation details:
- Hide file formats, algorithms, and data structures
- Prevent information leakage across module boundaries
- Design interfaces independent of internal implementation choices
- Changes to hidden information should not affect module users

### General-Purpose Design

Create modules slightly more general than immediate requirements:
- Identify the core functionality separate from specific use cases
- Provide clean interfaces not tied to particular scenarios
- Balance generality with simplicity - avoid over-generalization
- Enable future use cases without interface changes

### Pull Complexity Downwards

Module implementers should handle complexity rather than pushing it to users:
- Complex configuration should have sensible defaults
- Error conditions should be handled internally when possible
- Provide simple interfaces even for complex underlying operations
- Users should not need to understand implementation details

### Design Quality Indicators

**Positive Signals:**
- Simple interfaces supporting complex operations
- Clear separation between interface and implementation
- Modules handle their own complexity internally
- Consistent abstractions across related functionality

**Warning Signals:**
- Information leakage between modules
- Temporal decomposition (code organized by execution order)
- Pass-through methods that add no value
- Special-purpose interfaces for general problems
- Conjoined methods requiring specific calling sequences

**Design Principle:** Great design reduces complexity for all developers who will work with the system. Simple interfaces that hide complex functionality are the hallmark of excellent software architecture.

### KISS Principles and Pragmatism - CRITICAL

**Core Philosophy**

**Simplicity over complexity.** Always choose the simplest, most direct solution rather than over-engineered complex approaches.

#### Pragmatic Principles
1. **Minimum viable solution first**: Implement the simplest solution that solves the problem
2. **Embrace simplicity**: Use straightforward solutions that solve the specific problem at hand
3. **Progressive improvement**: Implement basic functionality first, then gradually improve based on actual needs
4. **Readability over cleverness**: Clear, understandable code is more important than technical showmanship
5. **Utility validation**: Every implementation should pass the "Is this really necessary?" test

#### Anti-Over-Engineering Checklist
Before implementing any solution, ask yourself:
- ✅ **Is this the simplest way to solve the problem?**
- ✅ **Am I unnecessarily complicating the solution?**
- ✅ **Do users really need these additional features?**
- ✅ **Can other developers easily understand this implementation?**
- ✅ **Am I reinventing the wheel?**

#### Simplicity-First Strategy
- **Direct implementation**: Prioritize using built-in language and framework features
- **Standard patterns**: Use proven, industry-standard design patterns and established architectures
- **Progressive complexity**: Start simple, add complexity only when necessary
- **Deletion over addition**: Question the necessity of every feature, actively remove unnecessary code

#### Complexity Control
- **Single responsibility**: Each function and class should do only one thing
- **Short and focused**: Keep functions within 20-30 lines
- **Clear naming**: Use self-explanatory variable and function names
- **Prefer flat structure**: Keep code structure shallow and readable

#### Red Flag Warnings - Over-Engineering Signals
- Creating "generic" or "extensible" solutions to solve single problems
- Using complex design patterns to solve simple problems
- Adding "might need in the future" features
- Creating abstraction layers to handle only one implementation
- Using latest technologies just because they're new

#### Practical Examples of Over-Engineering vs Simple Solutions

**❌ WRONG - Overly Complex (Showing Off)**
```bash
# Complex, error-prone IFS while loop
while IFS= read -r target_dir; do
    [[ -z "$target_dir" ]] && continue
    # Complex validation logic
    if [[ -d "$target_dir" ]] && \
       ([[ -d "$target_dir/debug" ]] || \
        [[ -d "$target_dir/release" ]] || \
        [[ -f "$target_dir/.rustc_info.json" ]]); then
        target_dirs+=("$target_dir")
        # Complex size calculation with multiple fallbacks
        if command -v numfmt >/dev/null 2>&1; then
            size_human=$(numfmt --to=iec-i --suffix=B $size_bytes)
        elif command -v bc >/dev/null 2>&1; then
            size_human=$(echo "scale=2; $size_bytes / (1024^2)" | bc)MB
        else
            size_human="$size_bytes bytes"
        fi
    fi
done < <(fd "^target$" --type d . 2>/dev/null)
```

**✅ CORRECT - Simple and Direct**
```bash
# Simple array assignment and iteration
target_dirs=($(fd "^target$" --type d --hidden --no-ignore . 2>/dev/null))

for target_dir in "${target_dirs[@]}"; do
    echo "$target_dir"
done
```

**❌ WRONG - Over-Engineered Configuration**
```bash
# Unnecessary complexity for simple script
parse_config() {
    local config_file="${1:-$HOME/.script_config}"
    if [[ -f "$config_file" ]]; then
        while IFS='=' read -r key value; do
            case $key in
                dry_run) DRY_RUN="$value" ;;
                verbose) VERBOSE="$value" ;;
            esac
        done < "$config_file"
    fi
}
```

**✅ CORRECT - Simple Command Line Args**
```bash
# Direct, obvious approach
while [[ $# -gt 0 ]]; do
    case $1 in
        -d|--dry-run) DRY_RUN=true; shift ;;
        -v|--verbose) VERBOSE=true; shift ;;
    esac
done
```

**❌ WRONG - Unnecessary Abstraction**
```bash
# Creating "reusable" functions for one-time use
calculate_human_size() {
    local bytes=$1
    local units=("B" "K" "M" "G" "T")
    local unit_index=0
    
    while [[ $bytes -gt 1024 && $unit_index -lt 4 ]]; do
        bytes=$((bytes / 1024))
        unit_index=$((unit_index + 1))
    done
    
    echo "${bytes}${units[$unit_index]}"
}
```

**✅ CORRECT - Use Existing Tools**
```bash
# Use du directly for what it's designed for
du -sh "$target_dir"
```

**Remember**: Code is for solving problems, not for showcasing technical ability. Excellent code is simple, readable, and maintainable.

## Task Completion and Reporting

### Mandatory Response Requirement
**For ANY task (regardless of complexity), you must provide a summary response.** This ensures users receive acknowledgment and confirmation of completed work.

- **Simple Tasks**: Provide a one-sentence summary confirming completion
- **Complex Tasks**: Provide a comprehensive completion summary with detailed breakdown

### Response Guidelines
Provide clear feedback for every completed task by including:
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
**ESSENTIAL**: Consistently use modern, efficient tools:
- **Use fd** for all file discovery tasks
- **Use rg** for all text search operations  
- **Use sg** for all code structure analysis

**Standard Practice**: Use `fd` and `rg` as your primary search tools for optimal performance.

### Optimized Tool Usage
**Recommended Tools for Best Performance:**
- Use `fd` for file discovery tasks
- Use `rg` for text search operations
- Use `sg` for code structure analysis

**Preferred Search Examples:**
- **File search:** `fd "\.swift$"` for finding Swift files
- **Name search:** `fd "MyFile.swift"` for specific files
- **Text search:** `rg "pattern"` for content matching

### rg Usage Examples

#### **Basic Commands**
- Search recursively: `rg "pattern"`
- Case-insensitive: `rg -i "pattern"`
- Show line numbers: `rg -n "pattern"`
- Context lines: `rg -C 3 "pattern"`
- File types: `rg --type=py "pattern"`
- Files only: `rg -l "pattern"`
- Multiple patterns: `rg -e "err" -e "warn"`

#### **Common Pitfalls and Conflicts**
- **Multiline vs Context**: Cannot use `-U` with `-A`/`-B`/`-C` flags together
- **Invalid:** `rg -U -A 5 "pattern"` - This will fail
- **Invalid:** `rg -A 20 -B 5 "build.*service\|get.*configuration"` - Context with multiline regex fails
- **Correct:** Choose one approach:
  - For multiline: `rg -U "build(.|\n)*?service"`
  - For context: `rg -A 20 "build.*service" && rg -B 5 "get.*configuration"`
- **Multiple pattern syntax**: Use `\|` for OR in regex, not `-e` with complex patterns
- **Context with piping**: Context flags are lost when piping to other commands
- **Case sensitivity**: Default is case-sensitive; use `-i` for case-insensitive searches

### fd Usage Examples

#### **Essential Commands**
- Find files: `fd "pattern"`
- By extension: `fd -e js -e ts`
- Directories only: `fd --type d pattern`
- Include hidden: `fd --hidden pattern`
- Specific path: `fd "pattern" path/to/search`
- Execute on results: `fd pattern -x command`

#### **Common Use Cases**
- Find config files: `fd "config" --type f`
- Find test files: `fd "test" -e js -e ts`
- Find all JavaScript: `fd -e js -e jsx -e ts -e tsx`
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
3. **fd**: **Primary choice** for efficient file discovery
4. **rg**: **Primary choice** for fast plain-text pattern matching
5. **Glob**: File pattern matching (`**/*.ts`, `src/**/test*.py`)
6. **Read**: Specific known file paths, small-to-medium files
7. **Bash**: For executing fd/rg/sg commands and complex searches requiring counts, line numbers, or advanced filtering

**Note**: All command-line tools (fd, rg, sg) are executed through the Bash tool. The Bash tool is the interface for running these modern alternatives.

### File Discovery Strategy
**Recommended approach for file discovery:**
- **Preferred:** `fd "*.js" src/` for JavaScript files in src directory
- **Preferred:** `fd -e py test/` for Python files in test directory
- **Preferred:** `fd config --type f` for config files

### Discovery Process
1. **Use ast-grep first** for any code structure analysis:
   - Function definitions, calls, or usage patterns
   - Class definitions, inheritance, or method patterns
   - Import/require statements or module usage
   - Variable assignments, declarations, or references
   - Language-specific syntax patterns
2. **Fall back to Agent** for broad semantic exploration when ast-grep cannot handle the query
3. **Use rg only** for plain-text searches when ast-grep isn't applicable
4. **Use fd** for efficient file discovery based on names/paths
5. **Read** identified files for detailed analysis

### ast-grep First Strategy
**ALWAYS prioritize ast-grep when the search involves:**
- Code structure or syntax (functions, classes, methods, etc.)
- Language-aware pattern matching (not just text matching)
- Refactoring or code transformation needs
- Finding usage patterns across codebases
- Cross-language searches in polyglot repositories

**Example Decision Matrix:**
- "Find all TODO comments" - Use rg (plain text)
- "Find all function calls to `fetchData`" - Use ast-grep
- "Find all React hooks usage" - Use ast-grep
- "Find all class definitions extending BaseClass" - Use ast-grep
- "Find files containing string 'password'" - Use rg (plain text)
- "Find all import statements from specific module" - Use ast-grep
- "Find all .env files" - Use fd
- "Find Python test files" - Use fd

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

**Simplicity Reminder**: Choose the simplest search method that gets the job done. Use ast-grep for code structure, rg for simple text searches, and fd for file discovery. Avoid over-complicating search strategies.

## File Handling and Reading

### Targeted Information Retrieval
When searching for specific content, patterns, or definitions within a codebase, prefer using search tools in this order:
1. **ast-grep (`sg`)** for structural code patterns and syntax-aware searches
2. **Agent** for semantic understanding and complex multi-round discovery
3. **fd** for efficient file discovery and name-based searches
4. **rg** for fast plain-text pattern matching when structural search isn't applicable

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

**Simplicity Reminder**: Follow KISS principles in edit operations - prefer multiple simple edits over single complex ones. This improves reliability and maintainability.

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
- **Recommended**: Use `fd` for efficient file discovery: `fd pattern /path`
- **Alternative**: Use `rg` with line numbers for plain-text patterns: `rg -n "pattern" file`
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
5. **Self-Correction**: When an edit fails, break it into smaller, more granular `Edit` or `MultiEdit` calls and retry with a more focused approach.

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

### Security Principles

**Security must be integrated into the design process, not added as an afterthought.** Every implementation requires security consideration from the beginning.

### Core Security Requirements

**Input Security:**
- Validate all user inputs using whitelist approaches
- Sanitize data before processing or storage
- Use parameterized queries to prevent injection attacks
- Validate file paths to prevent directory traversal

**Data Protection:**
- Never hardcode secrets, passwords, or API keys
- Encrypt sensitive data in storage and transmission
- Use secure hashing functions (bcrypt, scrypt) for passwords
- Implement proper session management with appropriate timeouts

**Access Control:**
- Apply principle of least privilege
- Implement appropriate authentication and authorization
- Use CSRF tokens for state-changing operations
- Set proper Content-Security-Policy headers

**Error Handling:**
- Prevent information leakage through error messages
- Log security events for audit trails
- Handle deserialization of untrusted data carefully
- Use HTTPS/TLS for all sensitive communications

### Security Verification

Before code deployment, verify:
- No potential injection attack vectors exist
- Sensitive data is properly protected
- Access controls are sufficient for the functionality
- Error handling does not reveal internal system details
- Secure communication protocols are used throughout

**Security Principle:** Prevention costs significantly less than remediation. Design with security as a fundamental requirement, not an optional feature.

## Implementation Completeness Standards

### Strategic Implementation Approach

**Complete implementations reduce long-term complexity.** Partial implementations create technical debt that increases system complexity over time.

### Core Implementation Principles

**Full Functionality:** Implement complete, working solutions rather than placeholders.
- Provide actual implementations in all function bodies
- Handle error conditions and edge cases appropriately
- Include comprehensive input validation and error handling
- Create production-quality code that can be deployed immediately

**Minimize Technical Debt:** Create clean, maintainable implementations.
- Implement complete functionality in each module
- Break large features into smaller, immediately completable units
- Use external tracking (TodoWrite tool, GitHub issues) for future work
- Complete each module thoroughly before moving to the next

**Information Hiding:** Ensure implementations hide complexity appropriately.
- Internal error handling should not expose implementation details
- Provide simple interfaces even for complex underlying operations
- Handle edge cases internally rather than requiring callers to manage them
- Use appropriate abstractions to manage complexity

### Implementation Quality Standards

**Essential Requirements:**
- All functions contain actual logic, not stubs or placeholders
- Error conditions are handled gracefully with meaningful messages
- Input validation prevents invalid states
- Edge cases and boundary conditions are addressed
- Code follows established patterns within the codebase

**Quality Verification:**
- Implementation handles all specified requirements
- Error conditions are properly managed
- Code is ready for production deployment
- Edge cases are considered and handled
- Implementation is maintainable and follows project conventions


## Strategic Refactoring Principles

### Complexity Reduction Through Clean Architecture

**Refactoring should reduce system complexity by eliminating redundancy and improving design.** Multiple implementations for the same functionality increase complexity without providing value.

### Core Refactoring Approach

**Complete Replacement:** When introducing improved implementations, remove legacy versions entirely.
- Update all usage points to the new implementation
- Delete deprecated functions, classes, and modules completely
- Remove unused dependencies and imports
- Maintain single source of truth for each functionality

**Strategic Design Investment:** Refactor with long-term maintainability as the primary goal.
- Improve module interfaces to hide complexity
- Enhance information hiding within module boundaries
- Create more general-purpose solutions when appropriate
- Pull complexity downward into implementation details

### Refactoring Process

**Planning Phase:**
- Identify all usage points of existing implementation
- Design new approach that handles all current requirements
- Plan migration strategy for dependent components

**Implementation Phase:**
- Create complete replacement implementation
- Update all call sites systematically
- Remove old implementation and unused dependencies
- Verify functionality through comprehensive testing

**Verification Phase:**
- Ensure new implementation handles all original use cases
- Confirm improved design reduces overall system complexity
- Validate that interface changes improve usability

### Legacy Compatibility

When backward compatibility is essential (public APIs, external interfaces):
- Version interfaces clearly (v1, v2) with migration timelines
- Separate legacy and new implementations in distinct modules
- Maintain consistency by using either legacy or new approaches within components
- Establish clear deprecation and removal schedules

**Refactoring Principle:** Successful refactoring eliminates complexity by providing better abstractions and cleaner interfaces. The goal is a simpler, more maintainable system.

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

## Code Formatting and Structure

### Always Use External Tools for Formatting
- **Use automated formatting tools** consistently - rely on external CLI tools like `jq`, `prettier`, `black`, etc.
- After JSON edits: `jq . file.json > tmp && mv tmp file.json`
- After code edits: Use project-specific formatters (`npm run format`, `black`, `prettier`, etc.)
- This ensures consistent formatting and avoids human error

### Granular Edit Operations
**Break down large modifications into atomic, sequential edits:**
- **Single Edit**: Limit to 50-100 lines maximum
- **MultiEdit**: Use 3-5 operations maximum per call
- **Complex changes**: Use multiple `Edit`/`MultiEdit` calls in sequence
- **Failed edits**: Immediately break into smaller operations and retry

**Example approach:**
```
// DON'T: Large complex edit
Edit(entire_function)

// DO: Sequential granular edits
Edit(function_signature)
Edit(function_body_part1)
Edit(function_body_part2)
```

## Test Integrity Guidelines - CRITICAL

### Fundamental Test Principles

**Preserve test integrity and purpose.** Tests are living documentation of expected behavior and requirements.

#### When Tests Fail - Analysis Framework:
1. **Understand First**: What business scenario or requirement is this test validating?
2. **Root Cause Analysis**: Why is the test failing?
   - Legitimate bug in the implementation?
   - Missing or incorrect mock/stub configuration?
   - Architectural changes requiring test updates?
   - Test expectations that no longer match requirements?

3. **Fix Hierarchy** (in order of preference):
   - **Fix the implementation** if there's a genuine bug
   - **Update mock/stub setup** to reflect new architecture while preserving test intent
   - **Adjust test expectations** only if business requirements have genuinely changed
   - **Preserve** all assertions and verification logic
   - **Maintain** the original test purpose and intent

#### Test Modification Guidelines:
- **Preserve test purpose**: Each test validates specific business behavior - keep that intact
- **Maintain verification completeness**: Don't remove error checking, boundary validation, or behavioral assertions
- **Use proper mocking**: Configure mocks to simulate intended scenarios, don't work around them
- **Review impact**: Use version control diffs to ensure changes don't alter test intent

#### Examples of Correct vs Incorrect Approaches:

**Wrong Approach - Removing verification to make test pass:**
```
// Original test validates specific error handling
assertThat(error).isInstanceOf(SpecificException.class);
assertThat(error.getMessage()).contains("Expected error message");

// WRONG: Simplified to just check something happened
assertThat(mockService.wasCallMade()).isTrue();
```

**Correct Approach - Updating setup while preserving verification:**
```
// Configure mock to simulate the intended failure scenario
mockService.stubFailure(new SpecificException("Expected error message"));

// Keep the original verification logic intact
assertThat(error).isInstanceOf(SpecificException.class);
assertThat(error.getMessage()).contains("Expected error message");
```

#### Pre-Modification Checklist:
Before changing any test, ask:
1. **"What business behavior is being validated here?"**
2. **"Am I preserving that validation in my changes?"**
3. **"Could I fix this by updating the code or mock setup instead?"**
4. **"Will my changes maintain the same level of confidence in the system?"**

#### Red Flags - Stop and Reconsider:
- Removing specific error type or message checks
- Simplifying complex test scenarios
- Commenting out assertions
- Changing test data to avoid edge cases
- Replacing detailed verification with generic checks

**Remember**: Tests failing often indicates real issues. Treat test failures as signals to investigate, not obstacles to bypass.

## General Interaction

Claude Code will directly apply proposed changes and modifications using available tools rather than describing them and asking for manual implementation. This ensures efficient and direct workflow.

### Natural Communication Style

**Communicate naturally and collaboratively.** Build genuine working relationships.

#### Key Principles
- **Authentic communication**: Use natural language and varied responses
- **Be genuine**: Acknowledge mistakes naturally, express thought processes clearly
- **Collaborate as peers**: Suggest improvements, show initiative proactively
- **Learn and adapt**: Thank users for feedback, demonstrate continuous improvement
- **Stay professional**: Maintain boundaries while being genuinely engaged

**Remember**: Good collaboration requires mutual respect and honest communication.
