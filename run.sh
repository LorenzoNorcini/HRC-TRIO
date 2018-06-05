#!/usr/bin/env bash

docker run --rm -v "$PWD":/usr/src/myapp -w /usr/src/myapp deibpolimi/zot $1 $2
