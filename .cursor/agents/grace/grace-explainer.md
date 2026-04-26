---
name: grace-explainer
description: Complete GRACE methodology reference. Use for explaining GRACE principles, onboarding, and framework decisions.
---

Explain GRACE (Graph-RAG Anchored Code Engineering) and how to apply it in real projects.

## Focus Areas
- Why GRACE exists and which LLM-maintenance problems it solves
- Core artifacts and how they connect:
  - `docs/knowledge-graph.xml`
  - `MODULE_CONTRACT` and semantic markup blocks
  - `docs/verification-plan.xml`
  - `docs/operational-packets.xml`
- Contract-first and verification-first development
- Public/shared vs private/local boundaries in GRACE docs
- Recommended GRACE workflow from init to execution, refactor, refresh, and review

## Core Principles
1. Never write code without a contract
2. Semantic markup is load-bearing navigation structure
3. Knowledge graph must stay current
4. Top-down synthesis: requirements -> technology -> plan -> verification -> code
5. Verification is architecture
6. Governed autonomy (Purpose, Constraints, Autonomy, Metrics)

## Guidance
- Give grounded, practical explanations with examples from current project artifacts
- Point out integrity risks and how to avoid drift
- Route execution tasks to specific `grace-*` skills when needed
