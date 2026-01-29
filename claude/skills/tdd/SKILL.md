---
name: tdd
description: test-driven development workflow
---

# Testing Guidelines

- Fail fast: Write test unit case first, expect it to fail.
- Fix bug: reproduce the bug first, then fix the root cause.

## Core Principle
**Tests are living documentation.** Preserve test integrity and purpose.

## When Tests Fail - Analysis Framework

1. **Understand**: What business behavior is this test validating?
2. **Root Cause**: Why failing?
   - Bug in implementation?
   - Missing/incorrect mock setup?
   - Requirements changed?

3. **Fix Hierarchy** (preference order):
   - Fix the implementation if genuine bug
   - Update mock/stub setup while preserving intent
   - Adjust expectations only if requirements truly changed
   - **Always preserve** assertions and verification logic

## Modification Guidelines
- **Preserve purpose**: Each test validates specific behavior - keep it
- **Maintain completeness**: Don't remove error checking or boundary validation
- **Proper mocking**: Configure mocks correctly, don't work around them

## Pre-Modification Checklist
1. What behavior is being validated?
2. Am I preserving that validation?
3. Could I fix code/mock setup instead?
4. Will this maintain confidence in the system?

## Red Flags - Stop and Reconsider
- Removing specific error type/message checks
- Simplifying complex test scenarios
- Commenting out assertions
- Changing test data to avoid edge cases
- Replacing detailed verification with generic checks

**Remember**: Test failures are signals to investigate, not obstacles to bypass.

## Agent Workflow Verification

- Make changes verifiable through dev/test commands
- Tests passing should conclude agent work - don't require human testing for each change
- Use realistic seed/dummy data for offline testing
