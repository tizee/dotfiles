Review the code and improve code quality.

When refactoring or modifying the codebase, follow these established design principles:

### SOLID Principles
- **Single Responsibility Principle (SRP)**: Each function/class should have one reason to change. Keep functions focused on a single task.
- **Open/Closed Principle (OCP)**: Code should be open for extension but closed for modification. Use function extraction to make code more extensible.

### Code Quality Guidelines
- **KISS Principle**: Keep it simple. Break complex logic into smaller, understandable functions.
- **DRY Principle**: Don't repeat yourself. Extract common logic into reusable functions.
- **Function Length**: Keep functions short and focused (ideally under 50 lines following Code Complete 2 guidelines).

### Refactoring Guidelines
- **Extract Functions**: When a function exceeds 50 lines or handles multiple concerns, extract subfunctions.
- **Eliminate Duplication**: Look for repeated code patterns and extract them into shared utilities.
- **Improve Readability**: Use descriptive function names that clearly indicate their purpose.
- **Maintain Testability**: Small, focused functions are easier to test and debug.

