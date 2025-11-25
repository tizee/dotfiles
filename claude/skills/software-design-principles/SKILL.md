---
name: software-design-principles
description: Provides strategic software design principles, code quality standards, security requirements, and refactoring guidelines. Use when making architectural decisions, evaluating code quality, or planning refactoring efforts.
---

# Software Design Principles and Code Quality Standards

## Strategic Software Design Principles

### Core Philosophy: Managing Complexity

**The primary challenge in software development is managing complexity.** Every design decision should evaluate whether it reduces or increases the overall complexity burden on developers.

### Deep Modules: Simple Interfaces, Powerful Functionality

Strive to create modules with simple interfaces that hide significant functionality:

**Deep Module Characteristics:**
- Simple, clean interface requiring minimal knowledge from users
- Complex implementation hidden behind the interface
- High functionality-to-interface-complexity ratio
- Users can accomplish significant work with minimal interface calls

**Create Deep Modules Instead:**
- Design interfaces that provide substantial functionality with minimal complexity
- Encapsulate implementation details behind clean abstractions
- Enable users to accomplish tasks without understanding internal concepts

### Strategic vs Tactical Programming

**Strategic Programming:** Invest time in good design to create maintainable systems.
- Primary goal: produce excellent design that also works
- Invest 10-20% additional time in design considerations
- Focus on long-term maintainability over immediate completion
- Continuously refactor to improve design quality

**Choose Strategic Programming:** Invest in sustainable design for long-term success.
- Balance immediate functionality with excellent design
- Continuously refactor to maintain code quality
- Build systems that become easier to modify over time

### Information Hiding

Modules should encapsulate design decisions and implementation details:
- Hide file formats, algorithms, and data structures
- Prevent information leakage across module boundaries
- Design interfaces independent of internal implementation choices
- Changes to hidden information should not affect module users

### General-Purpose Design

Create modules slightly more general than immediate requirements:
- Identify the core functionality separate from specific use cases
- Provide clean interfaces not tied to particular scenarios
- Balance generality with simplicity - avoid over-generalization
- Enable future use cases without interface changes

### Pull Complexity Downwards

Module implementers should handle complexity rather than pushing it to users:
- Complex configuration should have sensible defaults
- Error conditions should be handled internally when possible
- Provide simple interfaces even for complex underlying operations
- Users should not need to understand implementation details

### Design Quality Indicators

**Positive Signals:**
- Simple interfaces supporting complex operations
- Clear separation between interface and implementation
- Modules handle their own complexity internally
- Consistent abstractions across related functionality

**Warning Signals:**
- Information leakage between modules
- Temporal decomposition (code organized by execution order)
- Pass-through methods that add no value
- Special-purpose interfaces for general problems
- Conjoined methods requiring specific calling sequences

**Design Principle:** Great design reduces complexity for all developers who will work with the system. Simple interfaces that hide complex functionality are the hallmark of excellent software architecture.

### KISS Principles and Pragmatism - CRITICAL

**Core Philosophy**

**Simplicity over complexity.** Always choose the simplest, most direct solution rather than over-engineered complex approaches.

#### Pragmatic Principles
1. **Minimum viable solution first**: Implement the simplest solution that solves the problem
2. **Embrace simplicity**: Use straightforward solutions that solve the specific problem at hand
3. **Progressive improvement**: Implement basic functionality first, then gradually improve based on actual needs
4. **Readability over cleverness**: Clear, understandable code is more important than technical showmanship
5. **Utility validation**: Every implementation should pass the "Is this really necessary?" test

#### Anti-Over-Engineering Checklist
Before implementing any solution, ask yourself:
- ✅ **Is this the simplest way to solve the problem?**
- ✅ **Am I unnecessarily complicating the solution?**
- ✅ **Do users really need these additional features?**
- ✅ **Can other developers easily understand this implementation?**
- ✅ **Am I reinventing the wheel?**

#### Simplicity-First Strategy
- **Direct implementation**: Prioritize using built-in language and framework features
- **Standard patterns**: Use proven, industry-standard design patterns and established architectures
- **Progressive complexity**: Start simple, add complexity only when necessary
- **Deletion over addition**: Question the necessity of every feature, actively remove unnecessary code

#### Complexity Control
- **Single responsibility**: Each function and class should do only one thing
- **Short and focused**: Keep functions within 20-30 lines
- **Clear naming**: Use self-explanatory variable and function names
- **Prefer flat structure**: Keep code structure shallow and readable

#### Red Flag Warnings - Over-Engineering Signals
- Creating "generic" or "extensible" solutions to solve single problems
- Using complex design patterns to solve simple problems
- Adding "might need in the future" features
- Creating abstraction layers to handle only one implementation
- Using latest technologies just because they're new

#### Practical Examples of Over-Engineering vs Simple Solutions

**❌ WRONG - Overly Complex (Showing Off)**
```bash
# Complex, error-prone IFS while loop
while IFS= read -r target_dir; do
    [[ -z "$target_dir" ]] && continue
    # Complex validation logic
    if [[ -d "$target_dir" ]] && \
       ([[ -d "$target_dir/debug" ]] || \
        [[ -d "$target_dir/release" ]] || \
        [[ -f "$target_dir/.rustc_info.json" ]]); then
        target_dirs+=("$target_dir")
        # Complex size calculation with multiple fallbacks
        if command -v numfmt >/dev/null 2>&1; then
            size_human=$(numfmt --to=iec-i --suffix=B $size_bytes)
        elif command -v bc >/dev/null 2>&1; then
            size_human=$(echo "scale=2; $size_bytes / (1024^2)" | bc)MB
        else
            size_human="$size_bytes bytes"
        fi
    fi
done < <(fd "^target$" --type d . 2>/dev/null)
```

**✅ CORRECT - Simple and Direct**
```bash
# Simple array assignment and iteration
target_dirs=($(fd "^target$" --type d --hidden --no-ignore . 2>/dev/null))

for target_dir in "${target_dirs[@]}"; do
    echo "$target_dir"
done
```

**❌ WRONG - Over-Engineered Configuration**
```bash
# Unnecessary complexity for simple script
parse_config() {
    local config_file="${1:-$HOME/.script_config}"
    if [[ -f "$config_file" ]]; then
        while IFS='=' read -r key value; do
            case $key in
                dry_run) DRY_RUN="$value" ;;
                verbose) VERBOSE="$value" ;;
            esac
        done < "$config_file"
    fi
}
```

**✅ CORRECT - Simple Command Line Args**
```bash
# Direct, obvious approach
while [[ $# -gt 0 ]]; do
    case $1 in
        -d|--dry-run) DRY_RUN=true; shift ;;
        -v|--verbose) VERBOSE=true; shift ;;
    esac
done
```

**❌ WRONG - Unnecessary Abstraction**
```bash
# Creating "reusable" functions for one-time use
calculate_human_size() {
    local bytes=$1
    local units=("B" "K" "M" "G" "T")
    local unit_index=0

    while [[ $bytes -gt 1024 && $unit_index -lt 4 ]]; do
        bytes=$((bytes / 1024))
        unit_index=$((unit_index + 1))
    done

    echo "${bytes}${units[$unit_index]}"
}
```

**✅ CORRECT - Use Existing Tools**
```bash
# Use du directly for what it's designed for
du -sh "$target_dir"
```

**Remember**: Code is for solving problems, not for showcasing technical ability. Excellent code is simple, readable, and maintainable.

## Code Quality Standards

### Before Making Changes
- Understand existing code patterns and style
- Identify test files and testing patterns
- Check for linting/formatting tools and follow conventions

### After Making Changes
- Run relevant tests when available (`npm test`, `pytest`, etc.)
- Run linting/formatting tools if configured
- Verify changes don't break existing functionality

### Security Principles

**Security must be integrated into the design process, not added as an afterthought.** Every implementation requires security consideration from the beginning.

#### Core Security Requirements

**Input Security:**
- Validate all user inputs using whitelist approaches
- Sanitize data before processing or storage
- Use parameterized queries to prevent injection attacks
- Validate file paths to prevent directory traversal

**Data Protection:**
- Never hardcode secrets, passwords, or API keys
- Encrypt sensitive data in storage and transmission
- Use secure hashing functions (bcrypt, scrypt) for passwords
- Implement proper session management with appropriate timeouts

**Access Control:**
- Apply principle of least privilege
- Implement appropriate authentication and authorization
- Use CSRF tokens for state-changing operations
- Set proper Content-Security-Policy headers

**Error Handling:**
- Prevent information leakage through error messages
- Log security events for audit trails
- Handle deserialization of untrusted data carefully
- Use HTTPS/TLS for all sensitive communications

#### Security Verification

Before code deployment, verify:
- No potential injection attack vectors exist
- Sensitive data is properly protected
- Access controls are sufficient for the functionality
- Error handling does not reveal internal system details
- Secure communication protocols are used throughout

**Security Principle:** Prevention costs significantly less than remediation. Design with security as a fundamental requirement, not an optional feature.

## Implementation Completeness Standards

### Strategic Implementation Approach

**Complete implementations reduce long-term complexity.** Partial implementations create technical debt that increases system complexity over time.

### Core Implementation Principles

**Full Functionality:** Implement complete, working solutions rather than placeholders.
- Provide actual implementations in all function bodies
- Handle error conditions and edge cases appropriately
- Include comprehensive input validation and error handling
- Create production-quality code that can be deployed immediately

**Minimize Technical Debt:** Create clean, maintainable implementations.
- Implement complete functionality in each module
- Break large features into smaller, immediately completable units
- Use external tracking (TodoWrite tool, GitHub issues) for future work
- Complete each module thoroughly before moving to the next

**Information Hiding:** Ensure implementations hide complexity appropriately.
- Internal error handling should not expose implementation details
- Provide simple interfaces even for complex underlying operations
- Handle edge cases internally rather than requiring callers to manage them
- Use appropriate abstractions to manage complexity

### Implementation Quality Standards

**Essential Requirements:**
- All functions contain actual logic, not stubs or placeholders
- Error conditions are handled gracefully with meaningful messages
- Input validation prevents invalid states
- Edge cases and boundary conditions are addressed
- Code follows established patterns within the codebase

**Quality Verification:**
- Implementation handles all specified requirements
- Error conditions are properly managed
- Code is ready for production deployment
- Edge cases are considered and handled
- Implementation is maintainable and follows project conventions

## Strategic Refactoring Principles

### Complexity Reduction Through Clean Architecture

**Refactoring should reduce system complexity by eliminating redundancy and improving design.** Multiple implementations for the same functionality increase complexity without providing value.

### Core Refactoring Approach

**Complete Replacement:** When introducing improved implementations, remove legacy versions entirely.
- Update all usage points to the new implementation
- Delete deprecated functions, classes, and modules completely
- Remove unused dependencies and imports
- Maintain single source of truth for each functionality

**Strategic Design Investment:** Refactor with long-term maintainability as the primary goal.
- Improve module interfaces to hide complexity
- Enhance information hiding within module boundaries
- Create more general-purpose solutions when appropriate
- Pull complexity downward into implementation details

### Refactoring Process

**Planning Phase:**
- Identify all usage points of existing implementation
- Design new approach that handles all current requirements
- Plan migration strategy for dependent components

**Implementation Phase:**
- Create complete replacement implementation
- Update all call sites systematically
- Remove old implementation and unused dependencies
- Verify functionality through comprehensive testing

**Verification Phase:**
- Ensure new implementation handles all original use cases
- Confirm improved design reduces overall system complexity
- Validate that interface changes improve usability

### Legacy Compatibility

When backward compatibility is essential (public APIs, external interfaces):
- Version interfaces clearly (v1, v2) with migration timelines
- Separate legacy and new implementations in distinct modules
- Maintain consistency by using either legacy or new approaches within components
- Establish clear deprecation and removal schedules

**Refactoring Principle:** Successful refactoring eliminates complexity by providing better abstractions and cleaner interfaces. The goal is a simpler, more maintainable system.
