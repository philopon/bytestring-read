#!/bin/bash

set -eu

test "$GHCVER" = "$BENCH_TARGET"

git config --global user.name "travis"
git config --global user.email "travis@example.com"

cabal configure --enable-benchmarks
cabal build bench
./dist/build/bench/bench -o bench.html

git clone -b gh-pages git@github.com:philopon/bytestring-read.git
cd bytestring-read

DEST=./continuous/$TRAVIS_BRANCH.html
mv ../bench.html $DEST
git add $DEST
git commit -m "continuous benchmark($TRAVIS_JOB_NUMBER)"
git push origin gh-pages
