# dokku-cancel-builds helper

This helper wraps the logic needed to terminate in-flight Dokku builds for a single app. The code is split into two layers:

- `remote/cancel-builds-core.sh` contains the cancellation routine and is designed to run on the Dokku host (making it simple to promote into a standalone plugin command later).
- `cancel-builds` is a thin client that discovers the Dokku host, streams the remote script over SSH, and invokes it with the requested app name.

## Usage

```bash
scripts/dokku-cancel-build/cancel-builds <app>
```

Optional flags:

- `--dry-run` – show the actions that would be taken without killing anything.
- `--host`, `--port`, `--user`, `--ssh-option` – override connection details.
- `--local` – execute the remote logic locally (useful for developing a Dokku plugin around the same implementation).

The remote routine will:

1. terminate `docker build` / `pack build` processes targeting the app image;
2. stop any builder containers still running (`/build` commands, pack phases, etc.);
3. remove the app deploy lock (`/home/dokku/<app>/.deploy.lock`).

When packaged as a plugin, the `cancel-builds-core.sh` implementation can be reused verbatim as the plugin command entrypoint.
