---
name: grace-verification
description: Design and enforce testing, traces, and log-driven verification for GRACE projects and keep verification-plan.xml trustworthy.
---

Design verification that autonomous agents can trust: deterministic first, observable and traceable where needed.

## Goals
- Prove outcome correctness
- Prove execution trajectory where relevant
- Leave actionable failure evidence for future agents

## Process
1. Load verification context (`requirements`, `technology`, `development-plan`, `verification-plan`, scoped source/tests).
2. Derive verification targets from contracts and flows.
3. Design observability and required stable markers.
4. Build/refresh `docs/verification-plan.xml` entries (`V-M-xxx`, scenarios, commands, markers, follow-up checks).
5. Choose evidence types per scenario:
   - deterministic assertions first
   - trace assertions when path matters
   - integration/smoke checks as needed
6. Implement/update tests and evidence hooks.
7. Apply verification level split:
   - module level
   - wave level
   - phase level
8. On failure, produce concise failure packet for handoff (`grace-fix`).

## Rules
- Do not replace strong deterministic checks with fuzzy evaluation
- Keep verification plan synchronized with real tests/commands/markers
- Improve observability when verification is weak
