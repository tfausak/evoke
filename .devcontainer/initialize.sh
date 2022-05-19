#! /usr/bin/env sh
set -o errexit -o xtrace

VOLUME=cabal-store
docker volume inspect "$VOLUME" \
  || docker volume create "$VOLUME"
