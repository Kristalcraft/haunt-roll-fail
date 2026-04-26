---
name: grace-plan
description: Run the GRACE architectural planning phase to design modules, contracts, data flows, and verification references.
---

Run the GRACE architectural planning phase.

## Prerequisites
- `docs/requirements.xml` exists with use cases
- `docs/technology.xml` exists with stack decisions
- `docs/verification-plan.xml` exists (or recreate template before finalizing)

## Process
1. Analyze requirements and derive module candidates.
2. Design module architecture:
   - module IDs/types/purposes
   - dependencies
   - public interfaces
   - tentative source/test paths
   - verification refs (`V-M-xxx`)
3. Draft verification surfaces:
   - critical scenarios
   - required logs/traces
   - module vs wave vs phase checks
4. Run mental walkthroughs of key flows.
5. Present architecture for user approval.
6. After approval, update:
   - `docs/development-plan.xml`
   - `docs/verification-plan.xml`
   - `docs/knowledge-graph.xml`

## Rules
- No code generation in planning phase
- Contract-first and verification-aware design
- Shared docs track public interfaces, not private helper internals
