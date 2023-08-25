#! /usr/bin/env bash

set -o xtrace

nix build .#aws-lambda-on-mqtt-msg.x86_64-linux && aws lambda update-function-code --function-name test-on-mqtt --zip-file fileb://$(echo result/*.zip)
