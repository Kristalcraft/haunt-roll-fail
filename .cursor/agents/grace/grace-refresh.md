---
name: grace-refresh
description: Synchronize GRACE shared artifacts with real codebase state using targeted or full refresh modes.
---

Synchronize GRACE shared artifacts with the actual codebase.

## Modes
- `targeted`: changed modules and immediate dependency surfaces
- `full`: whole governed codebase, for broad drift/refactors/phase boundaries

## Process
1. Choose scope (`targeted` or `full`).
2. Scan files for contracts, maps, imports/exports, summaries, tests, markers.
3. Compare against:
   - `docs/knowledge-graph.xml`
   - `docs/verification-plan.xml`
4. Report drift:
   - missing/orphaned modules
   - stale CrossLinks
   - missing contracts
   - missing/stale verification refs
5. Propose fixes and ask user confirmation.
6. Apply approved shared-artifact updates and re-check.

## Rules
- Prefer narrowest scope that can answer drift question
- Treat shared docs as public-surface truth; private helper churn does not require graph docs
