# syntax=docker/dockerfile:1.6

FROM ubuntu:22.04 AS build
ARG PGROLL_VERSION=0.14.2
ARG NODE_VERSION=20.11.1
ENV DEBIAN_FRONTEND=noninteractive
RUN --mount=type=cache,target=/var/cache/apt     --mount=type=cache,target=/var/lib/apt     apt-get update     && apt-get install --yes --no-install-recommends          curl ca-certificates xz-utils          build-essential pkg-config git          libgmp-dev libpq-dev zlib1g-dev     && update-ca-certificates
RUN curl -fsSL "https://nodejs.org/dist/v${NODE_VERSION}/node-v${NODE_VERSION}-linux-x64.tar.xz"       | tar -xJ -C /usr/local --strip-components=1

# Install ghcup, GHC and Cabal
ENV BOOTSTRAP_HASKELL_NONINTERACTIVE=1 \
    PATH=/root/.ghcup/bin:/root/.local/bin:$PATH
RUN --mount=type=cache,target=/root/.ghcup \
  curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
RUN --mount=type=cache,target=/root/.ghcup \
  . /root/.ghcup/env \
  && ghcup install ghc 9.8.4 \
  && ghcup set ghc 9.8.4 \
  && ghcup install cabal 3.12.1.0 \
  && ghcup set cabal 3.12.1.0
WORKDIR /workspace

COPY frontend/package.json frontend/package-lock.json ./frontend/
RUN --mount=type=cache,target=/root/.npm npm ci --prefix frontend

COPY hs-starter.cabal cabal.project cabal.project.freeze ./
RUN --mount=type=cache,target=/root/.ghcup \
    --mount=type=cache,target=/root/.cabal/store \
    --mount=type=cache,target=/root/.cabal/packages \
    . /root/.ghcup/env \
    && cabal update --index-state=2025-09-01T00:00:00Z
RUN --mount=type=cache,target=/root/.ghcup \
    --mount=type=cache,target=/root/.cabal/store \
    --mount=type=cache,target=/root/.cabal/packages \
    . /root/.ghcup/env \
    && cabal build --only-dependencies

COPY . .
RUN --mount=type=cache,target=/root/.npm npm run build --prefix frontend
RUN --mount=type=cache,target=/root/.ghcup \
    --mount=type=cache,target=/root/.cabal/store \
    --mount=type=cache,target=/root/.cabal/packages \
    . /root/.ghcup/env \
    && cabal build exe:hs-starter

RUN --mount=type=cache,target=/root/.ghcup \
    --mount=type=cache,target=/root/.cabal/store \
    --mount=type=cache,target=/root/.cabal/packages \
    . /root/.ghcup/env \
    && cabal install exe:hs-starter \
      --installdir /opt/app/bin \
      --install-method=copy \
      --overwrite-policy=always

RUN curl -sSL "https://github.com/xataio/pgroll/releases/download/v${PGROLL_VERSION}/pgroll.linux.amd64" \
      -o /opt/app/bin/pgroll \
    && chmod +x /opt/app/bin/pgroll

FROM ubuntu:22.04 AS runtime
ENV DEBIAN_FRONTEND=noninteractive \
    APP_HOME=/opt/app \
    OTEL_SERVICE_NAME=hs-starter \
    OTEL_EXPORTER_OTLP_ENDPOINT=http://otel-collector:4318 \
    OTEL_EXPORTER_OTLP_HEADERS=
WORKDIR ${APP_HOME}

RUN apt-get update \
    && apt-get install --yes --no-install-recommends \
         libgmp10 libtinfo6 libpq5 zlib1g ca-certificates \
    && rm -rf /var/lib/apt/lists/* \
 && update-ca-certificates

COPY --from=build /opt/app/bin/hs-starter /usr/local/bin/hs-starter
COPY --from=build /opt/app/bin/pgroll /usr/local/bin/pgroll
COPY --from=build /workspace/frontend/build ${APP_HOME}/frontend
COPY --from=build /workspace/db/pgroll ${APP_HOME}/db/pgroll
COPY --from=build /workspace/scripts ${APP_HOME}/scripts

EXPOSE 8080
ENTRYPOINT ["/bin/bash", "-lc"]
CMD ["hs-starter"]
