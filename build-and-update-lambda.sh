#! /usr/bin/env bash

EXECUTABLE="${1:?no cabal executable name given}"
FUNC_NAME="${EXECUTABLE##aws-lambda-}"

set -o xtrace

nix build .#$EXECUTABLE.x86_64-linux \
&& aws lambda update-function-code --function-name "test-$FUNC_NAME" --zip-file fileb://result/$EXECUTABLE.zip
