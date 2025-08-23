#!/bin/bash

set -e

BUILD_DIR=build
DIST_DIR=dist

mkdir -p ${BUILD_DIR}
mkdir -p ${DIST_DIR}

# Haskell
echo "Building Haskell..."
cabal build
cabal install --install-method=copy --installdir=${DIST_DIR} --overwrite-policy=always

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
