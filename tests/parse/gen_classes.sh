#!/usr/bin/env bash

set -eu

pushd tgz
for TARBALL in *.tgz; do
    tar xzf "${TARBALL}"
done
popd

pushd clazz
for CLAZZ in *.clazz; do
    cp "${CLAZZ}" "${CLAZZ%.clazz}.class"
done
popd
