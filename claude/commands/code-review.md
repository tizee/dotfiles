# Code Review Prompt

Analyze the code systematically and provide **actionable improvements**. Focus on these areas in priority order:

## 1. Code Smells Detection
Identify and flag these critical issues:
- **Long functions** (>20 lines deserve scrutiny, >50 lines require refactoring)
- **Deep nesting** (>3 levels indicates complexity problems)
- **Magic numbers/strings** (extract to named constants)
- **Duplicate code blocks** (extract to functions immediately)
- **Large classes** (violating Single Responsibility)

## 2. Naming Quality
- Functions should **verb-noun** express what they do: `calculateTotalPrice()` not `process()`
- Variables should reveal **intent**: `userAccountBalance` not `data`
- Avoid **mental mapping**: no `i`, `temp`, `obj` unless in tight loops
- Boolean names should be **predicates**: `isValid`, `hasPermission`

## 3. Function Design
- **One level of abstraction** per function
- **Extract till you drop**: if you can name a code block meaningfully, extract it
- **Minimize parameters** (<3 ideal, >5 signals design problems)
- **Eliminate flag arguments** (they violate SRP)

## 4. Error Handling
- **Fail fast** with clear error messages
- **Don't return null** - use Optional or throw exceptions
- **Consistent error handling** patterns across similar functions

## 5. Comments & Documentation
- **Remove redundant comments** that restate code
- **Explain WHY, not WHAT** - the code shows what, comments explain business logic
- **Update stale comments** or delete them

## Output Format
For each issue found:
1. **File and line reference**
2. **Specific problem** with code snippet
3. **Concrete solution** with improved code example
4. **Impact explanation** (readability/maintainability/performance)

Prioritize **high-impact, low-effort** improvements first.
