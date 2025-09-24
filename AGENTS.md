# Repository Guidelines

## Project Structure & Module Organization
Executable entry points live under `app/` (`Main.hs`, migration utilities) while reusable modules sit in `src/` by domain (e.g., `Starter/Database`). Database migrations belong in `db/pgroll/` with `.ledger` tracking order; anything generated (Squeal types, docs) should stay under `src/Starter/Database/Generated.hs`. Tests mirror runtime modules inside `test/`, grouping helpers under `Starter/Tests`.

## Build, Test, and Development Commands
Run `cabal build` for a full compile and `cabal test` to execute the tasty suite (property, unit, and Postgres smoke tests). Use `cabal run hs-starter` to launch the server skeleton and run `scripts/squealgen.sh` (which shells out to the upstream `squealgen` CLI) whenever migrations change. Start a Cabal repl with `cabal repl hs-starter` during development loops.

## Coding Style & Naming Conventions
Format Haskell with `ormolu` (`cabal run ormolu -- --mode inplace $(git ls-files '*.hs')`) before pushing. Stick to two-space indentation, GHC2021 extensions, and module prefixes of `Starter.<Area>`. Keep exported data types and constructors UpperCamelCase; local helpers may remain lowerCamelCase. Generated files must remain ASCII.

## Database & Migrations Workflow
Author schema changes in pgroll YAML first and append new files to `db/pgroll/.ledger`. Once the migration file is staged, run `scripts/squealgen.sh` to apply the pgroll stack in a disposable Postgres container and regenerate `src/Starter/Database/Generated.hs` via the upstream `squealgen` CLI. Never hand-edit `Starter.Database.Generated`; treat it as a build artifact.
All database interaction must use Squeal; do not introduce alternative PostgreSQL clients.

## Testing Guidelines
`cabal test` runs tasty with falsify properties and tmp-postgres-backed smoke checks. Property suites live under `Starter.Tests.Property`; integration helpers use `Starter.Tests.Db`. When introducing new DB features, add tmp-postgres coverage or document why it is skipped. Target >85% coverage once the suite expands and note any regressions in PRs.
For local coverage runs: ensure `dist-newstyle/cache/plan.json` exists (run `cabal build` once), then execute
```
export PATH="$(pg_config --bindir):$PATH"
UNIT_LIB_ID=$(jq -r '."install-plan"[] | select(."pkg-name" == "hs-starter" and ."component-name" == "lib") | ."id"' dist-newstyle/cache/plan.json)
UNIT_TEST_ID=$(jq -r '."install-plan"[] | select(."pkg-name" == "hs-starter" and (."component-name" | startswith("test:"))) | ."id"' dist-newstyle/cache/plan.json)
cabal test --enable-coverage --enable-per-component --coverage-for="${UNIT_LIB_ID}" --coverage-for="${UNIT_TEST_ID}" --test-show-details=streaming
```
CI uses `8c6794b6/hpc-codecov-action` to generate a Codecov JSON report and uploads it with `codecov/codecov-action`.

## Commit & Pull Request Guidelines
Follow `type: summary` messages (examples: `feat: add oauth login route`, `chore: regenerate squeal schema`). PRs should summarise migrations applied, commands run (`cabal build`, `scripts/squealgen.sh`), and include screenshots or curl output for API changes. Cross-link tracking issues and call out any follow-ups.

## Environment & Tooling Notes
Install pgroll, Docker, and the upstream `squealgen` CLI (see https://github.com/mwotton/squealgen) when working migrations; `scripts/squealgen.sh` expects all three plus a local Postgres image. Export sensitive settings via environment variables—not version control. GitHub Actions re-runs Cabal builds/tests with caching, so keep dependencies tidy and regenerate schema files before pushing.
