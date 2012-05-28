#!/bin/sh

cabal configure #--enable-library-profiling --enable-executable-profiling
cabal build
cabal copy --destdir=/tmp/voxseq
mkdir target
tar -czf target/voxseq.tar.gz /tmp/voxseq/
cp dist/build/voxseq/voxseq target/
cabal clean
