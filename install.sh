#!/usr/bin/env bash

set -e

HERE=$(cd `dirname $0`; pwd)

cd $HERE
cabal sandbox init
cabal install -j1
