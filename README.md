rebar3_erllambda
===========

Enable AWS Lambda functions to be written in Erlang


## Overview

`rebar3_erllambda` implements a
[Rebar3 Plugin](http://www.rebar3.org/docs/plugins) to facilitate the
development of [AWS Lambda](https://aws.amazon.com/lambda/)
functions, written entirely in Erlang.  This plugin works in concert with
the [`erllambda`](https://github.com/alertlogic/erllambda)
project, which documents how to write these functions and leverage the
capabilities provided.


## Getting Started

This section will cover the initial steps needed to bootstrap your
environment, generate a Lambda skeleton, deploy it into AWS and finally
execute it! You will then be able to take that skeleton and add your
specific functionality.


### Bootstraping Your Environment (Linux)

You will need basic development tools and packages, plus a working
installation of the [AWS CLI](https://aws.amazon.com/cli/).  Please follow
the instruction available and validate that this works.

Once the CLI is working, you will also need to ensure that you have at least
a profile defined in your `~/.aws/credentials` file: `default`

```
[default]
aws_access_key_id = AKIAJ...............
aws_secret_access_key = 29b....................................
```

Finally, the `rebar3_erllambda` plugin comes with a complete
[rebar3 template](http://www.rebar3.org/docs/using-templates) for getting
started with a running AWS Lambda skeleton. You will need to add the plugin
to the global rebar3 environment.  To do this add the following to the
`$HOME/.config/rebar3/rebar.config` file, which will make the plugin
globally available:

```
{plugins,
 [
  rebar3_erllambda
 ]}.
```

When you use this global plugin configuration later, you should ensure the
plugin is up-to-date:

```
$ rebar3 plugins upgrade rebar3_erllambda
===> Fetching rebar3_erllambda ({pkg,<<"rebar3_erllambda">>,<<"1.1.0">>})
...
===> Compiling rebar3_erllambda
```


### Generating A Skeleton

For each new Erlang Lambda function, you will want to start by using the
plugin to generate a skeleton for the project.  This will give you a fully
functional Lambda function written in Erlang, which you can update to
implement the desired functionality.

To start, ask the plugin to generate the skeleton files for the project:

```
$ rebar3 new erllambda name=eltest
===> Writing eltest/.edts
===> Writing eltest/.gitignore
===> Writing eltest/README.md
===> Writing eltest/rebar.config
===> Writing eltest/config/sys.config
===> Writing eltest/config/shell.config
===> Writing eltest/config/vm.args
===> Writing eltest/etc/eltest.template
===> Writing eltest/src/eltest.erl
===> Writing eltest/src/eltest.app.src
===> Writing eltest/test/.dummy
```

Once skeleton generation is complete, then you will need to initialize the
project directory as a git repo, check in the initial skeleton, and tag it.
This is necessary because the plugin uses the output of `git describe` as
the version number for your lambda function. To get this done, do the
following: 

```
git init
git add -A
git commit -m "Initial Skeleton"
git tag -m "Initial Skeleton" -a 0.0.0
```

Now you are ready to do your first build and deploy.

### Creating a package

As noted in [erllambda docs](https://github.com/alertlogic/erllambda#openssl-version) it's
advised to package the Erlang release with an ERTS built in an
environment that is very close to AWS Lambda environment.  There are two ways to get a proper ERTS: use a premade ERTS from a docker image or include a custom ERTS.

Using a premade ERTS from a docker image is highly recommended.  NIF dependencies (such as `jiffy`, which is used by `erllambda`) require the ERTS to be built in an environment very similar to the Lambda environment.  The docker image is specifically made for this.

#### Using the erllambda docker container

The [erllambda_docker repository](https://github.com/alertlogic/erllambda_docker) contains
docker image definitions which aim to replicate the live AWS Lambda
environment almost identically.

First, ensure that the `erllambda` docker image is available locally.  If not
please see [repo docs](https://github.com/alertlogic/erllambda_docker#obtain-an-image)
on how to obtain the images).

Next, run the required [rebar3 commands](#create-a-zip-package) **from within erllambda container**:

``` console
docker run -it --rm -v `pwd`:/buildroot -w /buildroot alertlogic/erllambda:20.3 bash
# ... run the rebar3 commands linked above
```

You might want to fetch all dependencies beforehand to delegate only
build step:

``` console
rebar3 get-deps
```

#### Include the ERTS manually

**NOTE:** This method won't work if there's a NIF dependency (such as
`jiffy`, which is used by `erllambda`) and release is built on OS
different from AWS Lambda container's OS.  You may get an error that says 
`invalid ELF header` or `{on_load_function_failed,jiffy}`.

Unless you already have pre-built ERTS, you can copy one from erllambda docker image:

1. Copy ERTS from the erllambda container with the following:

	``` console
	docker create --name erllambda erllambda:20.3
	docker cp erllambda:/usr/local/lib/erlang /tmp/20.3-lambda
	```

2. Configure `relx` to include a specific ERTS

	In `rebar.config` configure relx to include into release a specific ERTS:
	
	``` erlang
	{relx,
	 [
	  %% ...
	  {include_erts, "/tmp/20.3-lambda"},
	  {system_libs, "/tmp/20.3-lambda"},
	  %% ...
	 ]
	}.
	```

You can find more about cross-compilation in [rebar3 documentation](https://www.rebar3.org/docs/releases#section-cross-compiling).

#### Create a zip package

``` console
rebar3 get-deps
rebar3 compile
rebar3 release
rebar3 erllambda zip
```

#### Deploying via CFN

```
# Create bucket
aws s3api create-bucket --bucket erllambda-example

# place the artifacts into the bucket
aws s3 cp ./etc/eltest.template s3://erllambda-example/eltest/eltest.template
aws s3 cp ./_build/default/eltest-0.0.0.zip s3://erllambda-example/eltest/eltest-0.0.0.zip

# Create stack
aws cloudformation create-stack \
 --stack-name eltest-function \
 --capabilities "CAPABILITY_NAMED_IAM" \
 --template-url https://s3.amazonaws.com/erllambda-example/eltest/eltest.template \
 --parameters ParameterKey=artifactBucket,ParameterValue=erllambda-example \
              ParameterKey=baseStackName,ParameterValue=erllambda-eltest \
              ParameterKey=version,ParameterValue=0.0.0

# Wait until the stack creation is complete
aws cloudformation wait stack-create-complete --stack-name eltest-function
```

#### Deploying directly

If you do not want to bother with CloudFormation, you can create a standalone function with:

```
aws lambda create-function \
 --function-name eltest-function \
 --memory-size 1024 \
 --handler eltest \
 --zip-file fileb://_build/default/eltest-0.0.0.zip \
 --runtime provided \
 --role <role-arn>
```

#### Invoking your Erlang Lambda function

With the Lambda function now deployed into your development account, go into
the AWS Lambda console, find your function, and hit Test button (accepting the
default test event document for now).  

You can also invoke it from your console:

```
aws lambda invoke \
  --function-name us-east-1-erllambda-eltest-eltest \
  --log-type Tail \
  --payload '{"msg": "hello"}' \
  outputfile.txt
```

This should report that `"eltest:completed successfully"` and the CloudWatch Logs should show something like this:

```
creating necessary erllambda run dirs
OpenSSL is OpenSSL 1.0.1k-fips 8 Jan 2015
starting ErlangVM
20:57:54.350274
Exec: /var/task/erts-9.3.3.5/bin/erlexec -noshell -noinput -Bd -boot /var/task/releases/0.0.0/eltest -mode embedded -boot_var ERTS_LIB_DIR /var/task/erts-9.3.3.5/lib -config /tmp/erllambda_rundir/sys.config -args_file /tmp/erllambda_rundir/vm.args --
Root: /var/task

=INFO REPORT==== 24-Nov-2018::20:57:54 ===
Erllambda Starting at 1543093074927 with Env #{<SKIPPED>}
=INFO REPORT==== 24-Nov-2018::20:57:54 ===
initializing erllambda_poller for handler eltest[{supervisor,{local,erllambda_sup}},{started,[{pid,<0.500.0>},{id,erllambda_error_handler},{mfargs,{erllambda_error_handler,start_link,[]}},{restart_type,permanent},{shutdown,15000},{child_type,worker}]}]
[{supervisor,{local,erllambda_sup}},{started,[{pid,<0.501.0>},{id,erllambda_config_srv},{mfargs,{erllambda_config_srv,start_link,[]}},{restart_type,permanent},{shutdown,15000},{child_type,worker}]}]
[{application,erllambda},{started_at,nonode@nohost}]
[{application,eltest},{started_at,nonode@nohost}]
Invoke Next path 1543093075027 http://127.0.0.1:9001/2018-06-01/runtime/invocation/next
[{supervisor,{local,inet_gethost_native_sup}},{started,[{pid,<0.507.0>},{mfa,{inet_gethost_native,init,[[]]}}]}]
[{supervisor,{local,kernel_safe_sup}},{started,[{pid,<0.506.0>},{id,inet_gethost_native_sup},{mfargs,{inet_gethost_native,start_link,[]}},{restart_type,temporary},{shutdown,1000},{child_type,worker}]}]
START RequestId: 9cd3db93-f02b-11e8-9962-2b5064aed8e6 Version: $LATEST
[context@36312 aid="9cd3db93-f02b-11e8-9962-2b5064aed8e6"] 127.0.0.1 - - [24/Nov/2018:20:57:55 -0000] Invoke Next
Next returns, in invoke 1543093075041
Hello World!
Invoke Success path 1543093075041 http://127.0.0.1:9001/2018-06-01/runtime/invocation/9cd3db93-f02b-11e8-9962-2b5064aed8e6/response
Invoke Next path 1543093075042 http://127.0.0.1:9001/2018-06-01/runtime/invocation/next
END RequestId: 9cd3db93-f02b-11e8-9962-2b5064aed8e6
REPORT RequestId: 9cd3db93-f02b-11e8-9962-2b5064aed8e6	Init Duration: 709.98 ms	Duration: 1.85 ms	Billed Duration: 800 ms Memory Size: 512 MB	Max Memory Used: 84 MB	
```

That's it! You now have an AWS Lambda function running in Erlang.

### Updating Lambda code 

To update your AWS Lambda after making some changes, run the following:

```
rebar3 compile
rebar3 erllambda release
rebar3 erllambda zip

```
Then update the function directly from your machine:

```
aws lambda update-function-code \
 --function-name us-east-1-erllambda-eltest-eltest \
 --zip-file fileb://_build/default/eltest-$(git describe).zip
```

## Dependencies

The `rebar3_erllambda` application is built using
[`rebar3`](http://www.rebar3.org), and all other dependencies are
automatically pulled in when `rebar3_erllambda` is used in other projects
`rebar.config`.

## How to contribute

Contributions to this repo are always welcome.  If you have an idea for
improving the this or related components, please submit a
[github issue](https://github.com/alertlogic/rebar3_erllambda/issues),
or simply submit a PR directly that implements your improvement.

For complex changes or the introduction of a major feature, it is
beneficial to discuss ideas before implementing them, so that your efforts
can focus on pull requests that will be accepted more easily.

As you prepare your pull request, make sure that you follow the coding
conventions that exist in the files, and always make sure that all unit and
common tests run.  Please ensure that your contribution always adds to the
coverage percentage, and does not decrease it.


## How to report defects

If you encounter a problem, or simply have a question about using this
repo, please submit a
[github issue](https://github.com/alertlogic/rebar3_erllambda/issues).


## Initial setup, compilation and testing

*TLDR;* as long as your basic environment is setup, getting started
developing `rebar3_erllambda` should be as easy as forking the repo, and then:

```
git clone git@github.com:${USER}/rebar3_erllambda.git
cd rebar3_erllambda
git remote add upstream git@agithub.com:alertlogic/rebar3_erllambda.git
rebar3 get-deps compile
```

<!--- vim: sw=4 et ts=4 -->
