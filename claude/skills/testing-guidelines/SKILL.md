---
name: testing-guidelines
description: Provides comprehensive testing principles, test integrity guidelines, and best practices for writing, modifying, and fixing tests. Use when writing new tests, modifying existing tests, fixing failing tests, or when test-related decisions need to be made. Emphasizes preserving test purpose, maintaining verification completeness, and treating test failures as signals to investigate rather than obstacles to bypass.
---

# Testing Guidelines

Comprehensive testing principles and best practices to ensure test integrity and quality.

## Fundamental Test Principles

**Preserve test integrity and purpose.** Tests are living documentation of expected behavior and requirements.

## When Tests Fail - Analysis Framework

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

## Test Modification Guidelines

- **Preserve test purpose**: Each test validates specific business behavior - keep that intact
- **Maintain verification completeness**: Don't remove error checking, boundary validation, or behavioral assertions
- **Use proper mocking**: Configure mocks to simulate intended scenarios, don't work around them
- **Review impact**: Use version control diffs to ensure changes don't alter test intent

## Examples of Correct vs Incorrect Approaches

### Wrong Approach - Removing verification to make test pass

```java
// Original test validates specific error handling
assertThat(error).isInstanceOf(SpecificException.class);
assertThat(error.getMessage()).contains("Expected error message");

// WRONG: Simplified to just check something happened
assertThat(mockService.wasCallMade()).isTrue();
```

### Correct Approach - Updating setup while preserving verification

```java
// Configure mock to simulate the intended failure scenario
mockService.stubFailure(new SpecificException("Expected error message"));

// Keep the original verification logic intact
assertThat(error).isInstanceOf(SpecificException.class);
assertThat(error.getMessage()).contains("Expected error message");
```

## Pre-Modification Checklist

Before changing any test, ask:

1. **"What business behavior is being validated here?"**
2. **"Am I preserving that validation in my changes?"**
3. **"Could I fix this by updating the code or mock setup instead?"**
4. **"Will my changes maintain the same level of confidence in the system?"**

## Red Flags - Stop and Reconsider

- Removing specific error type or message checks
- Simplifying complex test scenarios
- Commenting out assertions
- Changing test data to avoid edge cases
- Replacing detailed verification with generic checks

## Core Principle

**Remember**: Tests failing often indicates real issues. Treat test failures as signals to investigate, not obstacles to bypass.
