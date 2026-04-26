---
name: grace-setup-subagents
description: Create GRACE subagent presets for current shell by reusing local agent-file conventions.
---

Create GRACE subagent files for the current shell using native conventions.

## Default Roles
1. `grace-module-implementer`
2. `grace-contract-reviewer`
3. `grace-verification-reviewer`
4. `grace-fixer`

## Process
1. Detect current shell and agent config conventions.
2. Find real local/global sample agent files to infer format.
3. Choose target directory (project-local by default).
4. Read canonical GRACE role prompts.
5. Render shell-specific agent files without changing role logic.
6. Report detected shell, target dir, created files, and assumptions.

## Rules
- Prefer copying real local format over inventing one
- Do not overwrite existing files without user intent
- This skill scaffolds execution support roles, not architecture-planning roles
