#! /usr/bin/env bash

set -o xtrace

nix build .#aws-lambda-on-http-req.x86_64-linux && aws lambda update-function-code --function-name test-on-http-req --zip-file fileb://$(echo result/*.zip)
