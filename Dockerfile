# syntax=docker/dockerfile:1

FROM ghcr.io/haskell/cabal:3.10 AS build
WORKDIR /workspace

COPY hs-starter.cabal cabal.project ./
RUN cabal update && cabal build --only-dependencies

COPY . .
RUN cabal install exe:hs-starter \
    --installdir /opt/app/bin \
    --install-method=copy \
    --overwrite-policy=always

FROM debian:bookworm-slim AS runtime
ENV APP_HOME=/opt/app \
    OTEL_SERVICE_NAME=hs-starter \
    OTEL_EXPORTER_OTLP_ENDPOINT=http://otel-collector:4318 \
    OTEL_EXPORTER_OTLP_HEADERS=
WORKDIR ${APP_HOME}

RUN apt-get update \
    && apt-get install --yes --no-install-recommends libgmp10 \
    && rm -rf /var/lib/apt/lists/*

COPY --from=build /opt/app/bin/hs-starter /usr/local/bin/hs-starter

EXPOSE 8080
ENTRYPOINT ["hs-starter"]
