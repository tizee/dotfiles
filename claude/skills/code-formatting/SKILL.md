---
name: code-formatting
description: Provides code formatting best practices and granular edit operation guidelines. Use when formatting code, making edits to files, or when determining the proper approach for code modifications. Emphasizes using external formatting tools and breaking down large modifications into atomic, sequential edits for reliability.
---

# Code Formatting and Structure

Guidelines for code formatting and structuring edit operations to ensure reliability and consistency.

## Always Use External Tools for Formatting

- **Use automated formatting tools** consistently - rely on external CLI tools like `jq`, `prettier`, `black`, etc.
- After JSON edits: `jq . file.json > tmp && mv tmp file.json`
- After code edits: Use project-specific formatters (`npm run format`, `black`, `prettier`, etc.)
- This ensures consistent formatting and avoids human error

### Common Formatting Tools by Language

**JavaScript/TypeScript:**
```bash
# Prettier
npx prettier --write file.js

# Project formatter
npm run format
```

**Python:**
```bash
# Black
black file.py

# isort for imports
isort file.py
```

**JSON:**
```bash
# jq for formatting
jq . file.json > tmp && mv tmp file.json
```

**Go:**
```bash
# gofmt
gofmt -w file.go
```

**Rust:**
```bash
# rustfmt
rustfmt file.rs
```

## Granular Edit Operations

**Break down large modifications into atomic, sequential edits:**

- **Single Edit**: Limit to 50-100 lines maximum
- **MultiEdit**: Use 3-5 operations maximum per call
- **Complex changes**: Use multiple `Edit`/`MultiEdit` calls in sequence
- **Failed edits**: Immediately break into smaller operations and retry

### Edit Strategy Pattern

**Instead of this:**
```
// DON'T: Large complex edit
Edit(entire_function)
```

**Do this:**
```
// DO: Sequential granular edits
Edit(function_signature)
Edit(function_body_part1)
Edit(function_body_part2)
```

### When to Break Down Edits

Break down edits when:
- Modifying more than 50-100 lines at once
- Making changes to multiple logical sections
- Refactoring complex functions or classes
- An edit operation fails - immediately retry with smaller chunks

### Benefits of Granular Edits

1. **Higher reliability**: Smaller edits have fewer points of failure
2. **Better error messages**: Easier to identify what went wrong
3. **Incremental progress**: Partial success is better than complete failure
4. **Easier to verify**: Each change can be validated independently

## Workflow Integration

**Typical formatting workflow:**

1. Make code changes using Edit/MultiEdit tools
2. Apply automated formatter for the language
3. Verify formatting is correct
4. Proceed with next changes

**Example:**
```bash
# After editing Python file
black src/module.py

# After editing JavaScript
npx prettier --write src/component.js

# After editing JSON config
jq . config.json > tmp && mv tmp config.json
```

## Key Principle

**Rely on tools for formatting, rely on granularity for reliability.** Never manually format code when automated tools are available, and always prefer smaller, focused edits over large, complex modifications.
