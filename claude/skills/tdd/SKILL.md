---
name: tdd
description: test-driven development workflow
---

# Testing Guidelines

- Fail fast: Write test first, expect it to fail.
- Fix bug: reproduce the bug first, then fix the root cause.

## Core Principle
**Tests are living documentation.** They describe what the product does, not how the code is wired internally. A test that breaks on a refactor (while behavior is unchanged) is a bad test -- it was coupled to implementation, not behavior.

## Behavior Over Implementation

Test **observable behavior** from the user/caller's perspective. Do not test internal mechanics.

**Ask before writing any test:**
- Does this test describe a product behavior, or does it just mirror the code structure?
- If I refactor the internals without changing what the user sees, does this test survive?
- Can a human read the test name and understand what requirement it verifies?

| Test Type | Example | Verdict |
|-----------|---------|---------|
| Behavior | "rejected tool call shows error in red" | Good -- tests what user sees |
| Behavior | "CJK character at buffer edge stays in bounds" | Good -- tests an invariant |
| Surface | "reducer calls `replace(state, field=X)`" | Bad -- coupled to implementation |
| Surface | "`_process_queue` is called 3 times" | Bad -- tests internal wiring |

**Surface tests** verify that code does what it already says it does. They add no confidence and break on every refactor. They reflect the current implementation, not the product design intention.

**Behavior tests** verify what the system promises to its users/callers. They survive refactoring because they are anchored to outcomes, not code paths.

## When Tests Fail - Analysis Framework

1. **Understand**: What product behavior is this test validating?
2. **Root Cause**: Why failing?
   - Bug in implementation?
   - Missing/incorrect mock setup?
   - Requirements changed?
   - Test was coupled to implementation that got refactored?

3. **Fix Hierarchy** (preference order):
   - Fix the implementation if genuine bug
   - Update mock/stub setup while preserving intent
   - Adjust expectations only if requirements truly changed
   - Rewrite the test at a higher level if it was a surface test
   - **Always preserve** the behavioral assertion

## Modification Guidelines
- **Preserve purpose**: Each test validates specific behavior - keep it
- **Maintain completeness**: Don't remove error checking or boundary validation
- **Proper mocking**: Configure mocks correctly, don't work around them
- **Elevate surface tests**: When touching a surface test, consider rewriting it as a behavior test

## Pre-Modification Checklist
1. What product behavior is being validated?
2. Am I preserving that validation?
3. Could I fix code/mock setup instead?
4. Will this maintain confidence in the system?
5. Would this test survive an internal refactor?

## Red Flags - Stop and Reconsider
- Removing specific error type/message checks
- Simplifying complex test scenarios
- Commenting out assertions
- Changing test data to avoid edge cases
- Replacing detailed verification with generic checks
- Asserting on internal method call counts instead of outcomes
- Test name describes code structure instead of product behavior

**Remember**: Test failures are signals to investigate, not obstacles to bypass.

## Agent Workflow Verification

- Make changes verifiable through dev/test commands
- Tests passing should conclude agent work - don't require human testing for each change
- Test output should read as an acceptance report -- a human glances at results and knows whether requirements are met
- Use realistic seed/dummy data for offline testing
