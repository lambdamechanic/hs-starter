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

Enable BuildKit on the Dokku host to take advantage of the Dockerfile cache mounts:

```bash
dokku config:set hs-starter DOCKER_BUILDKIT=1
```

With BuildKit active, the cached `apt` indexes are reused between builds and the image now provisions dependencies via the cache-aware mounts defined in the Dockerfile.

## Frontend

The single-page UI lives under `frontend/` and is built with SvelteKit + TypeScript. For local development:

```bash
cd frontend
npm install
npm run dev -- --host
```

The Docker build runs `npm run build` and places the static output in `/opt/app/frontend`; a custom `nginx.conf.sigil` serves those assets directly via Dokku before proxying API traffic to the Haskell application.

## Firebase configuration audit

Run `scripts/check-firebase-config.sh` after wiring this template to a Dokku app. It will:

- discover the Dokku app name from your `dokku` git remote (override with `scripts/check-firebase-config.sh <app>` if needed).
- fall back to `bash ~/.dokku/contrib/dokku_client.sh` when you use the stock Dokku client shim.
- require `FIREBASE_API_KEY` and `FIREBASE_PROJECT_ID`, deriving `FIREBASE_AUTH_DOMAIN` as `<project>.firebaseapp.com` when you haven’t configured a custom domain.
- curl `https://<authDomain>/__/firebase/init.json` and warn when the derived firebaseapp.com host isn’t served by Firebase yet.
- call the Identity Toolkit `accounts:createAuthUri` endpoint with a fake account to confirm the project/API key combination is valid.

Successful runs end with `All Firebase checks passed.` A derived auth domain shows a `⚠` warning until you deploy Firebase Hosting—which is normal if you only use Firebase Auth. Any `✖` output indicates a real configuration issue (missing env var, invalid project, etc.) and the script prints instructions for filling in Dokku config.

## End-to-end Firebase login test

A Playwright regression in `test/playwright/tests/me.spec.js` asserts that fetching `/me` kicks off the Firebase redirect flow and issues a successful `POST` request to `googleapis.com`. Run it with the official Playwright Docker image via:

```bash
scripts/playwright.sh
```

The helper script:

- pulls Firebase secrets from `dokku config hs-starter` when available;
- starts a disposable Postgres container and launches `cabal run hs-starter` on a random local port;
- exports `HS_STARTER_BASE_URL` for the browser session and cleans up processes/containers on exit;
- mounts `test/playwright` into the Playwright Docker image and executes `npm ci && npx playwright test`.

Provide the script with `cabal` and `docker`; it will handle the rest. You can forward additional flags to `npx playwright test` by appending them to the script invocation—for example `scripts/playwright.sh --headed`.
