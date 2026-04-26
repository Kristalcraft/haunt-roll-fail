---
name: grace-fix
description: Debug an issue using GRACE semantic navigation through graph, verification, contracts, and semantic blocks to apply a targeted fix.
---

Debug an issue using GRACE semantic navigation.

## Process
1. Locate likely module via `docs/knowledge-graph.xml` and `docs/verification-plan.xml`.
2. If available, use `grace module find/show` and `grace file show` to narrow scope.
3. Navigate to exact semantic block (`START_BLOCK_*`) and function contract.
4. Analyze mismatch between intended behavior (contract/verification) and actual code.
5. Apply minimal fix within semantic block boundaries.
6. Update metadata:
   - `CHANGE_SUMMARY`
   - function/module contracts if behavior changed
   - graph CrossLinks if dependencies changed
   - `docs/verification-plan.xml` if tests/markers/commands changed
7. Run relevant module-local verification.

## Rules
- Never fix code before reading its contract
- Never silently change architecture
- If issue is architectural, escalate to plan revision
