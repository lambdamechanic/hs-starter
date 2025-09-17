# hs-starter

Bootstrap Servant + Squeal application scaffolded with Cabal. Follow `AGENTS.md` for contributor guidance; roadmap and feature work are in progress.

## Observability

The application ships with OpenTelemetry instrumentation for both the WAI stack and Servant API. Configure the exporter by setting the standard OTLP environment variables before starting the service:

```bash
export OTEL_SERVICE_NAME=hs-starter
export OTEL_EXPORTER_OTLP_ENDPOINT=https://api.honeycomb.io
export OTEL_EXPORTER_OTLP_HEADERS="x-honeycomb-team=YOUR_API_KEY"
```

These variables are read during startup and passed through to the OpenTelemetry SDK; adjust them to point at your collector of choice.

## Database configuration

The service connects to Postgres using either a `DATABASE_URL` (preferred, e.g. when linking a Dokku Postgres service) or individual `DB_*` variables. Supported forms:

- `DATABASE_URL=postgres://USER[:PASSWORD]@HOST[:PORT]/DBNAME[?params]`
- Or set: `DB_HOST`, `DB_PORT`, `DB_NAME`, `DB_USER`, and optionally `DB_PASSWORD`.

When both are present, `DATABASE_URL` takes precedence.

## Deployment

Dokku executes the pgroll migrations during each deploy via `app.json`'s `scripts.dokku.predeploy` hook. Ensure the linked Postgres service exports a `DATABASE_URL`; the container image bundles the `pgroll` CLI so the hook can run `pgroll migrate db/pgroll --postgres-url "$DATABASE_URL" --schema public --pgroll-schema pgroll --complete` before web processes start.
