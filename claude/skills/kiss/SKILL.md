---
name: kiss
description: taste matters. review code as a senior software engineer
context: fork
agent: Explore
---

# Rule

Would a senior engineer say this is overcomplicated? If yes, list things that could be simplified

# Software Design Principles

## Core Philosophy
**Managing complexity is the primary challenge.** Every design decision should reduce complexity.

## Deep Modules
- Simple, clean interfaces hiding complex implementations
- Users accomplish tasks without understanding internals
- High functionality-to-interface-complexity ratio

## Strategic Programming
- Invest 10-20% extra time in design for long-term maintainability
- Continuously refactor to improve quality
- Build systems that become easier to modify over time

## Information Hiding
- Hide file formats, algorithms, data structures behind interfaces
- Prevent information leakage across module boundaries
- Changes to internals should not affect module users

## KISS - Critical
- **Minimum viable solution first**: Simplest solution that works
- **Readability over cleverness**: Clear code beats technical showmanship
- **Use existing tools**: Don't reinvent (`du -sh` vs custom size calculator)
- **Progressive complexity**: Start simple, add only when necessary

### Red Flags (Over-Engineering)
- Generic solutions for single problems
- Complex patterns for simple problems
- "Might need later" features
- Abstraction layers for one implementation

## Security Requirements
- Validate inputs (whitelist), sanitize before storage
- Never hardcode secrets; use parameterized queries
- Apply least privilege; handle errors without leaking details

## Implementation Standards
- Complete implementations, not stubs or placeholders
- Handle error conditions and edge cases
- Production-quality code deployable immediately

## Refactoring
- Remove legacy code entirely when replacing
- Update all usage points; delete unused dependencies
- Goal: simpler, more maintainable system

