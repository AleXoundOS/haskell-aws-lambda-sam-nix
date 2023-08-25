#! /usr/bin/env bash

set -o xtrace

aws lambda invoke --function-name test-on-mqtt --payload fileb://mqtt-test-msg.json aws-lambda-invoke-output.json && cat aws-lambda-invoke-output.json
