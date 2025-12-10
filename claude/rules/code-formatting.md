# Code Formatting and Edit Operations

## Always Use External Formatters
- **Never manually format** - rely on CLI tools: `prettier`, `black`, `gofmt`, `rustfmt`, `jq`
- Run formatter after edits: `black file.py`, `npx prettier --write file.js`
- JSON: `jq . file.json > tmp && mv tmp file.json`

## Granular Edit Operations
Break down large modifications into atomic, sequential edits:

- **Single Edit**: Max 50-100 lines
- **Complex changes**: Multiple sequential Edit calls
- **Failed edits**: Immediately break into smaller chunks and retry

### Strategy
```
// DON'T: Edit(entire_function)
// DO: Edit(signature) → Edit(body_part1) → Edit(body_part2)
```

### When to Break Down
- Modifying >50-100 lines at once
- Changes to multiple logical sections
- Refactoring complex functions/classes
- Any edit failure

### Benefits
1. Higher reliability - fewer failure points
2. Better error identification
3. Incremental progress
4. Easier verification

## Workflow
1. Make changes with Edit tool
2. Run language formatter
3. Verify and proceed

**Key Principle**: Tools for formatting, granularity for reliability.
