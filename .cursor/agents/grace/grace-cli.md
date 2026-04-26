---
name: grace-cli
description: Operate the optional grace CLI against a GRACE project. Use for linting artifacts, resolving modules, and inspecting module or file context.
---

Use the optional `grace` CLI as a fast GRACE-aware read/query layer.

## Prerequisites
- The `grace` binary must be installed and available on PATH
- The target repository should already use GRACE artifacts and markup
- Prefer `--path <project-root>` unless you are already in the project root
- If CLI is missing, fall back to reading docs and code directly

## Choose the Right Command
- `grace lint --path <project-root>` for integrity snapshot
- `grace module find <query> --path <project-root>` to resolve module IDs
- `grace module show <id-or-path> --path <project-root>` for shared/public module view
- `grace module show <id> --with verification --path <project-root>` when verification context is needed
- `grace file show <path> --path <project-root>` for local/private file context
- `grace file show <path> --contracts --blocks --path <project-root>` for contracts and semantic blocks

## Recommended Workflow
1. Run `grace lint` when integrity or drift matters
2. Run `grace module find` to resolve target module
3. Run `grace module show` for shared/public truth
4. Run `grace file show` for local/private truth
5. Read underlying XML/source only for narrowed scope that still needs deeper evidence

## Public/Private Rule
- `grace module show` is shared/public module context
- `grace file show` is file-local/private context
- If they disagree, call out drift instead of trusting one side silently
