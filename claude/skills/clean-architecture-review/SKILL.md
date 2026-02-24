---
name: clean-architecture-review
description: Review a codebase, PR, or module for clean architecture quality and production robustness. Use when Codex needs to detect cross-layer business logic mixing (domain/application/infrastructure/interface boundary violations), dependency direction leaks, SOLID principle problems, and KISS/over-engineering issues, then report findings prioritized with SRE-style severity levels (P0-P3) so users/agents can improve code quality for real-world production operation.
---

# Goal

Produce a senior-level architecture review that prioritizes production risk, not just style.

Focus on:
- Layer boundary integrity and dependency direction
- Cross-layer business logic chaos (domain/application/infrastructure/interface mixing)
- SOLID principle violations that create change risk
- KISS violations and over-engineering
- Production robustness risks caused by design choices

Read `references/review-checklist.md` before reviewing.

# Workflow

1. Map the architecture as implemented, not as intended.
- Identify actual layers/modules and dependency direction.
- Note ports/interfaces, adapters, use cases, domain models, infra implementations.
- Do not assume folder names match architecture.

2. Trace business flows end-to-end.
- Follow a few representative request/job/event paths from entrypoint to persistence/external calls.
- Mark where business decisions are made.
- Flag logic that crosses layer boundaries without clear ownership.

3. Review with three lenses in order.
- Clean architecture boundary and dependency rules
- SOLID change-resilience checks
- KISS simplification and over-engineering checks (use `kiss` skill framing)

4. Assess production impact and assign severity (`P0`..`P3`).
- Use the rubric in `references/review-checklist.md`.
- Prioritize blast radius, data correctness, recoverability, and operational risk.
- Prefer fewer high-confidence findings over many weak opinions.

5. Return findings-first output for action.
- List findings ordered by severity, highest first.
- Include concrete evidence (`path:line`) and why it matters in production.
- Suggest the smallest credible fix direction first.

# Evidence Rules

- Cite file references for every finding (`path:line` when possible).
- Separate confirmed facts from inference. Label inference explicitly.
- Prefer behavior and architecture risks over naming/style nits.
- Avoid abstract "clean code" commentary without impact.
- If information is missing (tests, runtime config, deployment assumptions), state that as a gap.

# Output Format

Use this structure:

## Findings

For each finding:
- `Severity`: `P0` / `P1` / `P2` / `P3`
- `Title`: short production-oriented statement
- `Evidence`: file references and observed behavior
- `Why it matters`: production failure mode, change risk, or reliability impact
- `Fix direction`: concrete, minimal next step (and optional strategic follow-up)

## Open Questions

- List unknowns that materially affect severity or correctness.

## Architecture Summary

- Brief summary of current layering quality, major strengths, and the top remediation priority.

# Review Priorities

- Escalate data corruption, security boundary breaks, and irreversible side effects.
- Escalate hidden coupling that makes safe production changes difficult.
- De-prioritize purely stylistic issues unless they clearly increase operational risk.
- Prefer simplification that reduces moving parts, interfaces, or branching complexity.

# KISS Lens (from `kiss`)

Apply this question repeatedly:
- Would a senior engineer say this is overcomplicated for the problem being solved?

Flag these patterns when they hurt maintainability or reliability:
- Generic solutions for a single concrete problem
- Complex patterns for simple workflows
- "Might need later" abstractions/features
- Extra layers/interfaces with only one implementation and no pressure for variation

# Severity Discipline

- Do not inflate severity to force attention.
- Do not hide production-significant architecture defects as "style".
- If unsure between two severities, choose the lower one and explain the uncertainty.
