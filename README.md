# Introduction

This is a template for building and deploying a haskell.nix project to AWS Lambda.

It shows the minimalist example for creating a Lambda function, which responds to HTTP queries at Function URL (which AWS provides for each Lambda function if configured).

# Prerequisites

[Nix](https://nixos.org/download.html) installed (multi-user installation) with [flakes enabled](https://nixos.wiki/wiki/Flakes#Enable_flakes).

# Developing

The template uses nix flake based on the IOHK [haskell.nix](https://input-output-hk.github.io/haskell.nix) infrastructure. And also it heavily relies on [aws-lambda-haskell-runtime project](https://theam.github.io/aws-lambda-haskell-runtime/01-getting-started.html).

Run `$ nix develop` in order to get all required dependencies and tools for development into your bash shell.

Warning! First invocation of `nix develop` (or `nix build`) will build lots of dependencies from sources which may take a few hours depending on the number of CPU cores. In fact, there are [instructions to avoid builds from source](https://input-output-hk.github.io/haskell.nix/tutorials/getting-started.html#setting-up-the-binary-cache), but I couldn't achieve this despite caches being set up and reachable...

When inside the `$ nix develop` you can use typical `cabal` commands like `cabal repl` and `cabal build` in order to develop and build the project manually. This is most suitable for development (but not for deployment).

# Deploying

## prerequisites

`$ nix develop` already provides AWS CLI (specified in [flake.nix](flake.nix)). But, as time goes on, Amazon might break backwards compatibility with the version pinned here, so it might require you to install AWS CLI system wide, or using something like `nix-shell -p awscli2` or upgrading `nixpkgs` (in flake.lock) altogether for the project, etc.

## prepare your AWS account for Lambda deployment

1. **create new AWS Access key**

    Log into your AWS account in web browser. Click on your account name in the top right corner and then click on `Security credentials` -- browser should navigate to IAM page. Find `Access keys` paragraph and click on `Create access key` button. Then copy `Access key ID` and `Secret access key` in some secure place -- they will be needed another step.

3. **create AWS Lambda role**

    On the same IAM page navigate to `Roles` section (from the menu on the left). Click on `Create role` button. Choose `Trusted entity type` to be `AWS service` and `Use case` to be `Lambda`. Click on the `Next` button. Select `AWSLambdaBasicExecutionRole` and click on the `Next` button. Enter `lambda-role` in the `Role name` field. Click on the `Create role` button. More instructions are in [AWS Developer Guide](https://docs.aws.amazon.com/lambda/latest/dg/runtimes-walkthrough.html#runtimes-walkthrough-prereqs). Now on the `Roles` page click on `lambda-role` and you will text under `ARN`, which will be needed later when managing functions with AWS CLI.
   _[It seems to be possible to configure roles using AWS CLI](https://github.com/jesuspc/aws-lambda-haskell#deployment), but I haven't tested this._

5. **configure AWS CLI**

    Run `$ aws configure` - it will ask you to enter `Access key ID` and `Secret access key`. Also it asks for [region](https://docs.aws.amazon.com/quicksight/latest/user/regions.html) and [AWS CLI output format](https://docs.aws.amazon.com/cli/latest/userguide/cli-usage-output-format.html) (I recommend `table`). As a result it creates configuration files in `$HOME/.aws/`.

## build the zip bundle for AWS Lambda

`$ nix build .#aws-lambda.x86_64-linux`

As a result, symlink `./result` will be created to a folder with zip archive, containing custom bootstrap runtime with all required libraries.

## create AWS Lambda function with Function URL

1. **create Lambda function**

    `$ aws lambda create-function --function-name test-hs-runtime-standalone --zip-file fileb://$(echo result/*.zip) --handler standaloneHandler --runtime provided --role arn:aws:iam::123456789012:role/lambda-role` (replace `arn:aws:iam::123456789012:role/lambda-role` with your ARN obtained earlier)

    Note that handler must match the one, specified in haskell code as the first argument to `addStandaloneLambdaHandler`.

3. **create Function URL**

    `$ aws lambda create-function-url-config --function-name test-hs-runtime-standalone --auth-type NONE`

    Save the returned URL for further `curl` requests.

4. **add permission to invoke Lambda function by HTTP request at specific URL**

    `$ aws lambda add-permission --function-name test-hs-runtime-standalone --statement-id allow-invokeFunctionUrl-publicly --action lambda:InvokeFunctionUrl --principal "*" --function-url-auth-type NONE`

    Note that everybody will be able to access this URL, invoking your function, because none Auth type is specified here.

## uploading updated Lambda function to AWS

4. `$ nix build .#aws-lambda.x86_64-linux && aws lambda update-function-code --function-name test-hs-runtime-standalone --zip-file fileb://$(echo result/*.zip)`

# Testing

Just use your Function URL obtained earlier. Example:

`$ curl -i https://1234567890abcdefghijklmnopqrstuv.lambda-url.eu-north-1.on.aws/?param=go`

The Lambda function should respond with date it was loaded on AWS and the value of `param` HTTP query parameter. The date should stay the same among multiple invocations, clearly demonstrating that AWS does not load/unload the binary on each invocation, but keeps it running and waiting for the next one.

If `param` HTTP query parameter is missing, the function should crash with an error message in stdout, captured by AWS and displayed in [AWS CloudWatch](https://eu-north-1.console.aws.amazon.com/cloudwatch/home?region=eu-north-1#logsV2:live-tail) for the chosen Lambda function.

# Testing of a Lambda function without HTTP API

`$ aws lambda invoke --function-name test-on-mqtt --payload $(echo '{ "keyA": "value1" }' | base64) aws-lambda-invoke-output.json && cat aws-lambda-invoke-output.json`
`$ aws lambda invoke --function-name test-on-mqtt --payload fileb://mqtt-test-msg.json aws-lambda-invoke-output.json && cat aws-lambda-invoke-output.json`
