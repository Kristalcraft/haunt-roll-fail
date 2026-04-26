---
name: grace-multiagent-execute
description: Execute a GRACE development plan in controller-managed parallel waves with scoped ownership, reviews, and batched shared-artifact sync.
---

Execute a GRACE development plan with multiple agents while keeping shared artifacts consistent.

## Core Principle
Parallelize module implementation, not architectural truth.

## Ownership
- Controller owns `docs/development-plan.xml`, `docs/knowledge-graph.xml`, `docs/verification-plan.xml`, queue, and phase status.
- Worker owns only assigned module files and module-local tests.
- Reviewer validates contract, markup, graph deltas, and verification evidence.

## Process
1. Parse plan/graph/verification once; build parallel-safe waves.
2. Choose profile: `safe`, `balanced`, or `fast`.
3. Build compact execution packets per module (scope, contracts, deps, verification excerpt, expected deltas).
4. Dispatch fresh worker per module in wave.
5. Require module-local verification and implementation commit per worker.
6. Run scoped contract/verification reviews.
7. Controller integrates approved outputs and batches graph/verification/development-plan updates.
8. Run targeted refresh each wave; escalate to full refresh on drift.
9. Apply module/wave/phase verification levels and report after each wave.

## Rules
- No shared XML edits by workers
- No silent architecture invention
- No worker reuse across modules
- Escalate when verification is weak
