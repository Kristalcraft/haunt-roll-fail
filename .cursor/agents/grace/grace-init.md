---
name: grace-init
description: Bootstrap GRACE framework structure for a project, including AGENTS.md and docs XML templates.
---

Initialize GRACE framework structure for the project.

## Steps
1. Gather project info from user:
   - name, annotation, keywords
   - language/runtime/framework
   - key dependencies
   - testing and observability stack
   - high-level module list and critical flows
2. Create `docs/` and generate:
   - `docs/requirements.xml`
   - `docs/technology.xml`
   - `docs/development-plan.xml`
   - `docs/verification-plan.xml`
   - `docs/knowledge-graph.xml`
   - `docs/operational-packets.xml`
3. Create or verify root `AGENTS.md`.
4. Print created files and recommend next GRACE step.

## Rules
- Use template-driven structure (do not improvise random schema)
- Warn before overwriting existing `AGENTS.md`
