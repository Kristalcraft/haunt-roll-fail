---
name: grace-execute
description: Execute the full GRACE development plan step by step with controller-managed packets, scoped reviews, verification, and sequential commits.
---

Execute the development plan step by step, generating code for each pending module with validation and commits.

## Prerequisites
- `docs/development-plan.xml` with ImplementationOrder
- `docs/knowledge-graph.xml`
- `docs/verification-plan.xml` with module-level checks
- If present, use `docs/operational-packets.xml` as canonical packet/delta schema
- If plan/graph/verification are missing, stop and route to the corresponding GRACE setup skills

## Core Principle
Keep execution sequential, with disciplined context handling and verification.

## Process
1. Parse plan/graph/verification once and build a controller execution queue.
2. For each approved step, process one module:
   - implement only packet scope
   - preserve GRACE markup/contracts
   - run module-local verification
   - produce graph and verification delta proposals
   - commit implementation immediately after passing checks
3. Run scoped review, fix critical issues, and rerun affected checks.
4. Apply shared artifact updates centrally:
   - `docs/knowledge-graph.xml`
   - `docs/verification-plan.xml`
   - step/phase status in `docs/development-plan.xml`
5. After each phase, run broader checks, refresh, and optional broader review.
6. Print progress after each step and a final execution summary.

## Rules
- Never skip a failing step
- Never silently deviate from contracts or required verification evidence
- Prefer step-level checks during generation and broader checks at phase boundaries
