#!/bin/bash

set -eu

test "$GHCVER" = "$BENCH_TARGET"

cabal configure --enable-benchmarks
cabal build bench
git clone -b gh-pages git@github.com:philopon/bytestring-read.git
./dist/build/bench/bench -o bytestring-read/continuous.html
cd bytestring-read
git add continuous.html
git commit -m "continuous benchmark($TRAVIS_JOB_NUMBER)"
git push origin gh-pages
