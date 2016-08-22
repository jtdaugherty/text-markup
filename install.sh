#!/usr/bin/env bash

set -e

HERE=$(cd `dirname $0`; pwd)

cd $HERE
cabal sandbox init
cabal install -j2 --only-dependencies --enable-tests
cabal install -j1 --enable-tests
