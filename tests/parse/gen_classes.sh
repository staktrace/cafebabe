#!/usr/bin/env bash

set -eu

pushd tgz
for TARBALL in *.tgz; do
    tar xzf "${TARBALL}"
done
popd
