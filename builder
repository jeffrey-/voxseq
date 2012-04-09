#!/bin/sh

cabal configure
cabal build
cabal copy --destdir=/tmp/voxseq
mkdir target
tar -czf target/voxseq.tar.gz /tmp/voxseq/
cp dist/build/voxseq/voxseq target/
cabal clean
