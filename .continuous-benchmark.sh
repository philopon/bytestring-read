#!/bin/bash

set -eu

cabal configure --enable-benchmarks
cabal build bench
git clone -b gh-pages git@github.com:philopon/bytestring-read.git
./dist/build/bench/bench -o bytestring-read/continuous/$GHCVER.html
cd bytestring-read
git add continuous
git commit -m "continuous benchmark on $GHCVER ($TRAVIS_JOB_NUMBER)"
git push origin gh-pages
