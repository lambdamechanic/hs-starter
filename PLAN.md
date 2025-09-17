# Project Plan

## Completed
- Stage 1 – Foundation: ensure CI, build tooling, baseline Servant app, squeal schema, roboservant fuzz, falsify, postgres test scaffolding
- Stage 2 – Database & Migrations: implement OAuth-related schema/migrations and update squealgen flow
- Stage 3 – OAuth Flow: wire dummy OAuth authorization callback, server routes, and persistence using the squeal backend
- Stage 4 – Observability & Docker: add Honeycomb/OpenTelemetry logging hooks and produce Dokku-ready Docker image
- Stage 5 – Test Coverage & CI Enhancements: enforce coverage ratchet in GitHub workflow, ensure dependency caching and Postgres availability in CI

## Upcoming
- Stage 6 – Dokku Deployment: finalize Docker image, document env vars, and configure migration hook for dokku-postgres
