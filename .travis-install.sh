#!/bin/bash

cabal update

if [ "$GHCVER" = "$BENCH_TARGET" ]; then
  cabal install --only-dependencies --enable-tests --enable-benchmarks
else
  cabal install --only-dependencies --enable-tests
fi
