---
name: grace-refactor
description: Refactor GRACE-governed code safely while keeping contracts, graph, verification plan, and semantic markup synchronized.
---

Refactor a GRACE project without architecture or verification drift.

## Supported Refactors
- rename, move, split, merge, extract, interface-tighten, path-only

## Process
1. Classify refactor and capture source/target scope plus invariants.
2. Build a refactor packet with approved deltas (contract, graph, verification).
3. Apply smallest safe refactor in code and tests.
4. Preserve/update GRACE markup and contracts.
5. Synchronize shared artifacts:
   - `docs/development-plan.xml`
   - `docs/knowledge-graph.xml`
   - `docs/verification-plan.xml`
6. Verify by blast radius (module -> integration -> phase as needed).
7. Run scoped review and targeted refresh; escalate if wider drift appears.

## Rules
- Never silently invent new architecture
- Refactor is complete only when code and shared artifacts are aligned
