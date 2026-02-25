# Severity Rubric (P0-P3)

Use production impact and recoverability, not personal preference.

## P0: Critical (production unsafe now)

Use when the design can directly cause severe outages, security incidents, or irreversible data corruption, especially when hard to detect or recover.

### Typical Examples

- Business invariants bypassed because logic is split across controller + repository + DB trigger in conflicting ways
- Domain rules enforced only in one adapter path while other paths write data directly
- Layer leakage causing secret handling, authorization, or tenant scoping to be skipped in some code paths
- Retry + side effect design causing duplicate charges/orders without idempotency boundary

## P1: High (likely production incident or frequent breakage)

Use when the architecture strongly increases probability of incidents, incorrect behavior, or unsafe changes.

### Typical Examples

- Domain logic depends on infrastructure SDK/framework types, making behavior hard to test and change
- Application/use-case layer contains transport-specific branching duplicated across endpoints/jobs
- Repository implementations decide business outcomes instead of returning domain-relevant data
- Cross-module circular dependencies that regularly force risky changes

## P2: Medium (maintainability and change-risk issue)

Use when the system can run, but the design increases complexity, slows safe changes, or makes defects more likely.

### Typical Examples

- SRP violations in services that combine orchestration, validation, persistence, and presentation mapping
- Excess abstractions (interfaces/factories/strategies) with no current variation pressure
- Inconsistent boundary ownership causing repeated logic duplication

## P3: Low (quality issue, limited near-term risk)

Use when the issue is real but lower-impact, localized, or primarily readability/consistency with weak operational consequence.

### Typical Examples

- Minor dependency direction inconsistency without current behavioral risk
- Small KISS violations that do not yet affect correctness or operations
