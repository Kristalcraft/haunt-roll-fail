---
name: grace-status
description: Show current GRACE project health: artifacts, graph and verification integrity, codebase metrics, and suggested next actions.
---

Show the current state of the GRACE project.

## Report Sections
1. Artifact status:
   - `AGENTS.md`
   - `docs/requirements.xml`
   - `docs/technology.xml`
   - `docs/development-plan.xml`
   - `docs/verification-plan.xml`
   - `docs/knowledge-graph.xml`
   - `docs/operational-packets.xml`
2. Codebase metrics:
   - source/test counts
   - files with/without MODULE_CONTRACT
   - semantic block pairing
   - log-marker coverage
3. Graph + verification health:
   - missing/orphaned entries
   - stale verification refs
4. Recent CHANGE_SUMMARY entries
5. Suggested next action (`grace-plan`, `grace-verification`, `grace-execute`, `grace-refresh`, etc.)

## Rule
If available, include `grace lint --path <project-root>` signals as a fast integrity snapshot.
