# Code Review: Complexity Management Focus

Principles:
- Correctness & invariants
- Simplicity (KISS > DRY)
- Testability / verifiability
- Maintainability (low coupling, high cohesion)
- Performance (measure first)

Analyze code systematically to **reduce complexity** and improve long-term maintainability. Focus on these areas in priority order:

## 1. Module Depth Analysis

### Basic Code Smells Detection

First, identify and flag these critical quality issues:
- **Long functions** (>20 lines deserve scrutiny, >50 lines require refactoring)
- **Deep nesting** (>3 levels indicates complexity problems)
- **Magic numbers/strings** (extract to named constants)
- **Duplicate code blocks** (extract to functions immediately)
- **Large classes** (violating Single Responsibility)

### Shallow Module Detection
Identify shallow modules that add complexity without value:
- **Pass-through methods** (just call another method without adding logic) → **Merge or eliminate**
- **Wrapper classes** with 1:1 method mapping → **Remove unnecessary abstraction**
- **Utility functions** that wrap single operations → **Use original operation directly**
- **Configuration classes** that just hold getters/setters → **Use simple data structures**

**Good**: `FileManager.save(data, options)` hiding complex file handling, validation, backup
**Bad**: `DataProcessor.process(data)` that just calls `data.transform()`

## 2. Information Hiding Violations
Look for implementation details leaking across boundaries:
- **File format knowledge** in multiple places → **Centralize in one module**
- **Database schema** referenced by UI components → **Add proper abstraction layer**
- **Algorithm details** exposed in interfaces → **Hide behind simple operations**
- **Error codes** from internal systems bubbling up → **Translate to domain errors**

**Example Fix**: Instead of `getUserById(id).getTable().getConnection()`, provide `getUserData(id)`

## 3. Strategic vs Tactical Programming Indicators
Flag tactical shortcuts that increase long-term complexity:
- **Hardcoded values** scattered through code → **Extract to configuration**
- **Copy-paste modifications** instead of abstraction → **Create reusable components**
- **Workarounds** that bypass existing systems → **Fix root cause or improve system**
- **Quick hacks** with TODO comments → **Implement proper solution**

**Red Flag**: Any comment starting with "TODO", "HACK", "FIXME" older than 2 weeks

## 4. General-Purpose vs Special-Purpose Design
Identify over-specialized code that should be generalized:
- **Similar functions** with slight variations → **Create parameterized version**
- **Hardcoded assumptions** about data format → **Make format configurable**
- **Business logic** mixed with technical implementation → **Separate concerns**
- **Special cases** handled by duplicating code → **Extend general solution**

**Example**: Instead of `saveUserToFile()`, `saveProductToFile()`, create `saveEntityToFile(entity, type)`

## 5. Complexity Distribution Problems
Look for complexity pushed upward to callers:
- **Configuration burden**: Users must set 10+ parameters → **Provide smart defaults**
- **Error handling burden**: Callers must catch 5+ exception types → **Handle internally or simplify**
- **State management burden**: Users must call methods in exact sequence → **Make order-independent**
- **Knowledge burden**: Must understand internals to use → **Hide implementation details**

**Example Fix**: Instead of requiring users to call `connect()`, `authenticate()`, `setOptions()`, `initialize()`, provide `createClient(config)`

## 6. Critical Red Flags
Watch for these complexity multipliers:

### Interface Pollution
- **Flag parameters** that change method behavior → **Split into separate methods**
- **Output parameters** that modify inputs → **Return new objects**
- **Method chains** requiring specific order → **Create fluent builder or single method**

### Information Leakage
- **Same constant** defined in multiple files → **Central constants file**
- **Database fields** referenced in UI code → **Use domain objects**
- **File paths** hardcoded in multiple places → **Configuration system**

### Temporal Coupling
- **Methods that must be called together** → **Combine into single operation**
- **Setup/teardown** required around operations → **Use RAII or try-with-resources**
- **State synchronization** between objects → **Encapsulate in single object**

## 7. Function Design Excellence

### Basic Function Design Rules
- **One level of abstraction** per function
- **Extract till you drop**: if you can name a code block meaningfully, extract it
- **Minimize parameters** (<3 ideal, >5 signals design problems)
- **Eliminate flag arguments** (they violate SRP)

### Advanced Complexity Principles
Beyond basic function rules, apply complexity principles:
- **Single concept per function**: If you can't name it clearly, it's doing too much
- **Hide complexity internally**: Complex logic inside, simple interface outside
- **Minimize cognitive load**: Caller shouldn't need to remember previous calls
- **Eliminate flag arguments**: `sendEmail(urgent: true)` → `sendUrgentEmail()`

## 8. Error Handling Philosophy

### Basic Error Handling
- **Fail fast** with clear error messages
- **Don't return null** - use Optional or throw exceptions
- **Consistent error handling** patterns across similar functions

### Advanced Error Design
Design errors out of existence when possible:
- **Defensive programming**: Handle null/empty gracefully instead of throwing
- **Sensible defaults**: Missing config → use reasonable default, don't crash
- **Idempotent operations**: Delete non-existent item → succeed silently
- **Recovery strategies**: Network failure → retry with backoff, don't just fail

**Example**: `delete(item)` should work whether item exists or not

## 9. Comments & Documentation

### Basic Documentation Quality
- **Remove redundant comments** that restate code
- **Explain WHY, not WHAT** - the code shows what, comments explain business logic
- **Update stale comments** or delete them

### Advanced Documentation Focus
Focus on non-obvious design decisions:
- **Design rationale**: Why this approach over alternatives
- **Performance implications**: When operations are expensive
- **Concurrency considerations**: Thread safety assumptions
- **Business constraints**: External requirements that shaped design
- **Interface contracts**: Precise behavior specifications

**Remove**: Comments that just restate code or describe implementation steps

## 10. Naming Quality
- Functions should **verb-noun** express what they do: `calculateTotalPrice()` not `process()`
- Variables should reveal **intent**: `userAccountBalance` not `data`
- Avoid **mental mapping**: no `i`, `temp`, `obj` unless in tight loops
- Boolean names should be **predicates**: `isValid`, `hasPermission`

## Output Format
For each issue found:
1. **File and line reference**
2. **Specific problem** with code snippet
3. **Concrete solution** with improved code example
4. **Impact explanation** (readability/maintainability/performance)

Prioritize **high-impact, low-effort** improvements first.
