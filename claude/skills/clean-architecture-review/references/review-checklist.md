# Clean Architecture Review Checklist

## Table of Contents

- Severity rubric (`P0`..`P3`)
- Layer boundary checks (clean architecture)
- Cross-layer business logic chaos patterns
- SOLID review checks
- KISS / over-engineering checks (from `kiss`)
- Production robustness checks
- Reporting heuristics

## Severity Rubric (`P0`..`P3`)

Use production impact and recoverability, not personal preference.

### `P0` Critical (production unsafe now)

Use when the design can directly cause severe outages, security incidents, or irreversible data corruption, especially when hard to detect or recover.

Typical examples:
- Business invariants bypassed because logic is split across controller + repository + DB trigger in conflicting ways
- Domain rules enforced only in one adapter path while other paths write data directly
- Layer leakage causing secret handling, authorization, or tenant scoping to be skipped in some code paths
- Retry + side effect design causing duplicate charges/orders without idempotency boundary

### `P1` High (likely production incident or frequent breakage)

Use when the architecture strongly increases probability of incidents, incorrect behavior, or unsafe changes.

Typical examples:
- Domain logic depends on infrastructure SDK/framework types, making behavior hard to test and change
- Application/use-case layer contains transport-specific branching duplicated across endpoints/jobs
- Repository implementations decide business outcomes instead of returning domain-relevant data
- Cross-module circular dependencies that regularly force risky changes

### `P2` Medium (maintainability and change-risk issue)

Use when the system can run, but the design increases complexity, slows safe changes, or makes defects more likely.

Typical examples:
- SRP violations in services that combine orchestration, validation, persistence, and presentation mapping
- Excess abstractions (interfaces/factories/strategies) with no current variation pressure
- Inconsistent boundary ownership causing repeated logic duplication

### `P3` Low (quality issue, limited near-term risk)

Use when the issue is real but lower-impact, localized, or primarily readability/consistency with weak operational consequence.

Typical examples:
- Minor dependency direction inconsistency without current behavioral risk
- Small KISS violations that do not yet affect correctness or operations

## Layer Boundary Checks (Clean Architecture)

Validate actual dependency direction and business logic placement.

### Expected Responsibility by Layer

#### Domain Layer

Own:
- Core business rules and invariants
- Domain entities/value objects/domain services
- Domain language and policies

Avoid:
- Framework types (HTTP requests/responses, ORM models, web context)
- SQL queries, network clients, filesystem calls
- Serialization formats and transport DTOs

#### Application / Use-Case Layer

Own:
- Orchestration of domain behavior
- Transaction boundaries (business-level)
- Coordination of ports/repositories/services
- Use-case-specific validation and workflow sequencing

Avoid:
- HTTP parsing/rendering details
- Raw infrastructure calls that bypass ports
- Business rules that belong inside domain objects/services

#### Interface / Adapter Layer

Own:
- Translate external inputs/outputs (HTTP, CLI, events) into application commands/results
- Map transport DTOs to use-case input and back

Avoid:
- Deep business decisions/invariants
- Persistence decisions

#### Infrastructure Layer

Own:
- Database, queue, cache, API clients, filesystem implementations
- Technical retries, serialization, connection management
- Implementing ports declared by inner layers

Avoid:
- Product/business decisions and branching rules
- Returning framework-specific models beyond the boundary

### Boundary Violation Red Flags

- Domain imports ORM/entity annotations tightly coupled to persistence behavior
- Domain or use-case code reads HTTP headers/cookies directly
- Controllers contain pricing, authorization policy, or state transition logic
- Repositories choose business status/outcome instead of data access concerns
- Infrastructure code silently applies business defaults/invariants
- DTOs leaking into domain method signatures
- Use cases depending on concrete adapters instead of ports/interfaces
- Circular dependencies between layers/modules

## Cross-Layer Business Logic Chaos Patterns

These are priority findings because they create hidden behavior and production inconsistency.

### Split Invariant Enforcement

Symptoms:
- Validation partly in controller, partly in service, partly in repository/DB
- Different entrypoints enforce different subsets of rules

Production risk:
- Inconsistent writes, bypassed checks, hard-to-debug incidents

### Hidden Business Decisions in Infrastructure

Symptoms:
- Retry policy changes business semantics (e.g., duplicate side effects)
- Repository auto-fills status/tenant/ownership rules
- Cache fallback changes authorization or pricing behavior

Production risk:
- Silent correctness bugs under failure modes

### Transport-Driven Domain Behavior

Symptoms:
- Domain logic branches on HTTP status, route names, request context
- Event payload schema details leak deep into core logic

Production risk:
- Fragile core model coupled to interface churn

### Persistence Model Equals Domain Model (without intent)

Symptoms:
- ORM model passed everywhere as domain object
- Lazy-loading/persistence semantics influence business rules

Production risk:
- Implicit I/O, transaction surprises, test unreliability

## SOLID Review Checks

Use SOLID as a change-risk lens, not a dogma checklist.

### S: Single Responsibility Principle

Flag when one class/function/module has multiple reasons to change:
- Business rules + persistence + transport mapping + logging all mixed
- "Manager"/"Service" objects doing orchestration and low-level I/O

Ask:
- Can one behavioral change force touching this unit for unrelated reasons?

### O: Open/Closed Principle

Flag when extension requires editing many existing branches:
- Type-code `if/else` scattered across modules
- Feature addition requires touching controller, service, repo, serializer in parallel due to missing boundary design

Nuance:
- Do not add plugin architectures prematurely. Prefer simple branching until variation is real.

### L: Liskov Substitution Principle

Flag when abstractions lie:
- Interface implementations violate expected invariants/error behavior
- "Repository" implementations return incompatible semantics (e.g., `None` vs exception vs partial object)

Production impact:
- Runtime surprises only under certain providers/environments

### I: Interface Segregation Principle

Flag bloated interfaces/ports:
- Consumers depend on methods they never use
- Shared interfaces force infra implementations to support irrelevant operations

KISS tie-in:
- Overly generic ports often indicate speculative abstraction.

### D: Dependency Inversion Principle

Flag dependency direction leaks:
- Use cases import concrete DB/SDK clients directly
- Domain depends on framework infrastructure utilities

Also flag fake DIP:
- Interface exists but concrete type is still constructed inside the use case, defeating inversion

## KISS / Over-Engineering Checks (from `kiss`)

Apply the `kiss` skill philosophy: manage complexity first.

### KISS Review Questions

- Is there a simpler design that preserves correctness and production safety?
- Does an abstraction hide complexity, or merely relocate it?
- Is the interface simpler than the implementation behind it?
- Is this complexity justified by current change pressure, scale, or reliability needs?

### Over-Engineering Red Flags

- Generic framework around one workflow and one implementation
- Multiple layers of factories/builders/strategies for trivial branching
- "Future-proofing" abstractions without a known second use case
- Configurability that weakens invariants or makes runtime behavior hard to reason about

### Good Complexity (Do Not Penalize)

Do not flag complexity if it clearly supports:
- Transactional correctness
- Idempotency and deduplication
- Fault isolation and retries with bounded semantics
- Security boundaries and tenancy isolation
- Observability needed for production debugging

## Production Robustness Checks

The goal is to decide whether the codebase can operate safely in real production conditions.

### Correctness and Data Safety

- Are business invariants enforced in one authoritative place?
- Can alternate entrypoints bypass core rules?
- Are state transitions explicit and validated?
- Are partial writes/side effects possible without compensation?

### Failure Handling

- Are retries/timeouts/circuit breaking placed in the right layer?
- Do retries change business semantics (duplicates, reordering)?
- Are failures surfaced loudly (fail fast) instead of silently defaulting?

### Operational Observability

- Can you trace a business flow across layers?
- Are errors/logs emitted at the right boundaries with enough context?
- Is critical behavior hidden in framework magic/ORM side effects?

### Change Safety

- Does changing a rule require touching many layers?
- Are interfaces stable and meaningful, or leaky and fragile?
- Can behavior be tested without full infrastructure boot?

## Reporting Heuristics

- Findings first, sorted by severity.
- Tie each finding to a concrete failure mode or change hazard.
- Prefer "move logic to X layer" over broad rewrite advice.
- If architecture is mostly sound, say so and highlight the strongest design choices.
- Distinguish immediate remediation from strategic refactor opportunities.
