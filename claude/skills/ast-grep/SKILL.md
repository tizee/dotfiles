---
name: ast-grep
description: Structural code pattern search and refactoring using ast-grep (sg). Use for finding functions, classes, methods, imports, or any code structure patterns. Supports syntax-aware analysis across multiple languages including JavaScript, TypeScript, Python, Ruby, Go, Rust, and more. Use when searching for code patterns based on structure rather than plain text.
---

# AST-Grep (sg) - Structural Code Search and Refactoring

ast-grep is a syntax-aware code search and refactoring tool. It understands code structure and semantics, making it superior to regex-based searches for code analysis.

## Quick Start

```bash
# Find function calls
sg -p 'functionName($$)' -l javascript .

# Find class definitions
sg -p 'class $NAME { $$ }' -l typescript .

# Find imports
sg -p 'import { $$ } from "$MODULE"' -l javascript .

# Interactive rewrite
sg -p '$OLD_PATTERN' --rewrite '$NEW_PATTERN' -l python --interactive .
```

## When to Use ast-grep

Use `sg` (ast-grep) instead of plain regex or text search when:

- **Structural code patterns** are involved (e.g., finding all function calls, class definitions, or method implementations)
- **Language-aware refactoring** is required (e.g., renaming variables, updating function signatures, or changing imports)
- **Complex code analysis** is needed (e.g., finding all usages of a pattern across different syntactic contexts)
- **Cross-language searches** are necessary (e.g., working with both JavaScript and TypeScript in a monorepo)
- **Semantic code understanding** is important (e.g., finding patterns based on code structure, not just text)

## Command Templates

### Basic Search Pattern
```bash
sg -p '$PATTERN' -l $LANGUAGE $PATH
```

### Search with Context
```bash
sg -p '$PATTERN' -l $LANGUAGE -C 3 $PATH  # Show 3 lines of context
sg -p '$PATTERN' -l $LANGUAGE -A 5 $PATH  # Show 5 lines after
sg -p '$PATTERN' -l $LANGUAGE -B 5 $PATH  # Show 5 lines before
```

### Interactive Rewrite
```bash
sg -p '$OLD_PATTERN' --rewrite '$NEW_PATTERN' -l $LANGUAGE --interactive $PATH
```

### JSON Output for Processing
```bash
sg -p '$PATTERN' -l $LANGUAGE --json $PATH
```

## Pattern Syntax Reference

- **`$VAR`** — matches any single node and captures it
- **`$$`** — matches zero or more nodes (wildcard)
- **`$$$`** — matches one or more nodes
- **Literal code** — matches exactly as written
- **Indentation insensitive** — matches regardless of whitespace/formatting

## Common Use Cases and Patterns

### JavaScript/TypeScript Patterns

```bash
# Find function calls
sg -p 'functionName($$)' -l javascript .

# Find class definitions
sg -p 'class $NAME { $$ }' -l typescript .

# Find method calls on objects
sg -p '$OBJ.$METHOD($$)' -l typescript .

# Find import statements
sg -p 'import { $$ } from "$MODULE"' -l javascript .

# Find React hooks
sg -p 'const [$STATE, $SETTER] = useState($$)' -l typescript .

# Find async functions
sg -p 'async function $NAME($$) { $$ }' -l javascript .
```

### Python Patterns

```bash
# Find function definitions
sg -p 'def $NAME($$): $$' -l python .

# Find class definitions
sg -p 'class $NAME: $$' -l python .

# Find method calls
sg -p '$OBJ.$METHOD($$)' -l python .

# Find imports
sg -p 'from $MODULE import $$' -l python .

# Find decorators
sg -p '@$DECORATOR' -l python .
```

### Ruby Patterns

```bash
# Find class definitions
sg -p 'class $NAME < $$; $$; end' -l ruby .

# Find method definitions
sg -p 'def $NAME($$); $$; end' -l ruby .

# Find method calls
sg -p '$OBJ.$METHOD($$)' -l ruby .
```

### General Patterns

```bash
# Find variable assignments
sg -p '$VAR = $$' -l $LANG .

# Find conditional statements
sg -p 'if $CONDITION { $$ }' -l $LANG .
```

## Supported Languages

- **Web**: `javascript`, `typescript`, `html`, `css`
- **Backend**: `python`, `ruby`, `go`, `rust`, `java`, `c`, `cpp`
- **Config**: `yaml`, `json`, `toml`
- **And many more** - see [full list](https://ast-grep.github.io/reference/languages.html)

## Integration Workflow

### Using ast-grep workflow:

1. **Identify** if the task involves structural code patterns or language-aware refactoring
2. **Determine** the appropriate language(s) to search
3. **Construct** the pattern using ast-grep syntax
4. **Run** ast-grep to gather precise structural information
5. **Use** results to inform code edits, refactoring, or further analysis

### Example Workflow

When asked to "find all service objects that call `perform` method":

1. **Run ast-grep search:**
   ```bash
   sg -p 'perform($$)' -l ruby app/services/
   ```
2. **Analyze** results structurally
3. **Use** Agent or Read tools for additional context if needed
4. **Make** informed edits based on structural understanding

## Advanced Usage

### Rewrite Patterns

```bash
# Simple rewrite
sg -p '$PROP && $PROP()' --rewrite '$PROP?.()' -l typescript

# Interactive rewrite session
sg -p '$OLD' --rewrite '$NEW' -l python --interactive

# Auto-apply all changes
sg -p '$OLD' --rewrite '$NEW' -l javascript --update-all
```

### Complex Patterns

```bash
# Find nested patterns
sg -p 'if ($COND) { if ($INNER) { $$ } }' -l javascript

# Find patterns with specific context
sg -p 'class $NAME { constructor($$) { $$ } }' -l typescript
```

## Key Benefits Over Regex

1. **Language-aware** — understands syntax and semantics
2. **Structural matching** — finds patterns regardless of formatting
3. **Cross-language** — works consistently across different languages
4. **Precise refactoring** — makes structural changes safely
5. **Context-aware** — understands code hierarchy and scope

## When NOT to Use ast-grep

Use plain text search (rg) instead when:
- Searching for plain text comments or documentation
- Looking for string literals or hardcoded values
- Searching across non-code files (markdown, text, etc.)
- Simple substring matching without structural context

Use fd instead when:
- Searching for files by name or path
- Finding files by extension
- Locating files in specific directories

## Decision Matrix

| Task | Tool to Use |
|------|-------------|
| Find all TODO comments | rg (plain text) |
| Find all function calls to `fetchData` | ast-grep |
| Find all React hooks usage | ast-grep |
| Find all class definitions extending BaseClass | ast-grep |
| Find files containing string 'password' | rg (plain text) |
| Find all import statements from specific module | ast-grep |
| Find all .env files | fd |
| Find Python test files | fd |
| Find all async functions | ast-grep |
| Search for error message text | rg (plain text) |

## Notes

- ast-grep is pre-installed and available, just like ripgrep (rg) and fd
- No availability checks needed
- All ast-grep commands are executed through the Bash tool
- Always prefer ast-grep for code structure analysis over regex-based approaches
