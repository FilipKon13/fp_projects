#!/bin/bash

set -e

mkdir -p build
mkdir -p dist

BUILD_DIR=build
DIST_DIR=dist

# Haskell
echo "Building Haskell..."
ghc -v -package-db network-3.2.8.0/dist-newstyle/packagedb/ghc-9.4.7 -package network -isrc -o ${DIST_DIR}/main -odir ${BUILD_DIR} src/Main.hs 

# C
# Compile if we have `rpc.h` header file, otherwise use provided binary (e.g. on `student`)
echo "Building C..."
if [ -f /usr/include/rpc/rpc.h ] ; then
    make --no-silent -C rpcc -f Makefile.kvstore kvstore_server -B
else
    echo "Skipping C compilation, using precompiled binary"
fi
cp rpcc/kvstore_server ${DIST_DIR}/rpc_server

# Python does not require compilation
echo "Building Python..."
cp python/server.py ${DIST_DIR}/http_server.py
