FROM ubuntu:22.04 AS build
ENV DEBIAN_FRONTEND=noninteractive
RUN apt-get update \
    && apt-get install --yes --no-install-recommends \
         curl ca-certificates xz-utils \
         build-essential pkg-config git \
         libgmp-dev libpq-dev \
    && rm -rf /var/lib/apt/lists/* \
 && update-ca-certificates

# Install ghcup, GHC and Cabal
ENV BOOTSTRAP_HASKELL_NONINTERACTIVE=1 \
    GHCUP_USE_XDG_DIRS=1 \
    PATH=/root/.ghcup/bin:$PATH
RUN curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh \
 && ghcup install ghc 9.8.4 \
 && ghcup set ghc 9.8.4 \
 && ghcup install cabal 3.12.1.0 \
 && ghcup set cabal 3.12.1.0 \
 && cabal --version \
 && ghc --version

WORKDIR /workspace

COPY hs-starter.cabal cabal.project ./
RUN cabal update \
 && cabal build --only-dependencies

COPY . .
RUN cabal install exe:hs-starter \
    --installdir /opt/app/bin \
    --install-method=copy \
    --overwrite-policy=always

FROM ubuntu:22.04 AS runtime
ENV DEBIAN_FRONTEND=noninteractive \
    APP_HOME=/opt/app \
    OTEL_SERVICE_NAME=hs-starter \
    OTEL_EXPORTER_OTLP_ENDPOINT=http://otel-collector:4318 \
    OTEL_EXPORTER_OTLP_HEADERS=
WORKDIR ${APP_HOME}

RUN apt-get update \
    && apt-get install --yes --no-install-recommends \
         libgmp10 libtinfo6 libpq5 ca-certificates \
    && rm -rf /var/lib/apt/lists/* \
 && update-ca-certificates

COPY --from=build /opt/app/bin/hs-starter /usr/local/bin/hs-starter

EXPOSE 8080
ENTRYPOINT ["hs-starter"]
