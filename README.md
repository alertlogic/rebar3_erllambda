rebar3_erllambda
===========

Enable AWS Lambda function to be written in Erlang


## Overview

The `rebar3_erllambda` implements a
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
a profil defined in your `~/.aws/credentials` file: `default` 

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
  {rebar3_erllambda,
   {git, "git@github/com:alertlogic/rebar3_erllambda.git",
    {branch, master}}}
 ]}.
```

When you use this global plugin configuration later, you should ensure the
plugin is up-to-date:

```
$ rebar3 plugins upgrade rebar3_erllambda
===> Fetching rebar3_erllambda ({git,
                                        "git@github.com:alertlogic/rebar3_erllambda.git",
                                        {branch,master}})
===> Compiling rebar3_erllambda
```


### Bootstraping Your Environment (Mac OS)

in addition to the instructions above, you will want to leverage the
[erllambda_docker](https://github.com/alertlogic/erllambda_docker)
project. To make development as simple as working directly on Linux.  

### Generating A Skeleton

For each new Erlang Lambda function, you will want to start by using the
plugin to generate a skeleton for the project.  This will give you a fully
functional Lambda function written in Erlang, which you can update to
implement the desired functionality.

To start, create a brand new directory in which you are going to development
your new Lambda function:

```
mkdir ~/src/eltest
cd eltest
```

**NOTE:** as noted above, you will need to create a bootstrap `rebar.config`
file in the empty directory as follows:

```
{plugins,
 [
  {rebar3_erllambda,
   {git, "git@github.com:alertlogic/rebar3_erllambda.git",
    {branch, master}}}
 ]}.
```

Once you have your project directory created, then you want to ask the
plugin to generate the skeleton files for the project:

```
$ rebar3 new erllambda -f name=eltest
===> Fetching rebar3_erllambda ({git,
                                        "github.com:alertlogic/rebar3_erllambda.git",
                                        {branch,master}})
===> Compiling rebar3_erllambda
===> Writing .edts
===> Writing .gitignore
===> Writing README.md
===> Writing rebar.config (forcibly overwriting)
===> Writing config/sys.config
===> Writing config/shell.config
===> Writing config/vm.args
===> Writing etc/eltest.template
===> Writing src/eltest.erl
===> Writing src/eltest.app.src
```

**NOTE:** for now you will need to pass the `-f` option to new to force it
to overwrite the bootstrap `rebar.config` file with the working version from
the template.

Once skeleton generation is complete, then you will need to initialize the
project directory as a git repo, check in the initial skeleton, and tag it.
This is necessary because the plugin uses the output of `git describe` as
the version number for your lambda function. To get this done, do the
following: 

```
rebar3 new -f erllambda name=eltest
git init
git add -A
git commit -m "Initial Skeleton"
git tag -m "Initial Skeleton" -a 0.0.0
```

Now you are ready to do your first build and deploy.  To do so, simply
execute the following:

```
rebar3 get-deps
rebar3 compile
rebar3 erllambda zip
```
#### Deploying via CFN
```
# Create bucket
aws s3api create-bucket --bucket erllambda-example

# place the artifacts into the bucket

aws s3 cp --profile integration --region us-east-1 \
	  $(pwd)/${art} s3://erllambda-example/user/eltest/$(basename ${art});
	  
# Create stack 
aws --profile default --region us-west-2 \
 cloudformation create-stack --stack-name user-eltest \
 --capabilities "CAPABILITY_NAMED_IAM" \
 --template-url https://s3.us-west-2.amazonaws.com/erllambda-example/user/eltest/search-fetcher-0.0.0.template \
 --parameters ParameterKey=artifactBucket,ParameterValue=uerllambda-example ParameterKey=baseStackName,ParameterValue=user
 
# Wait until the stack creation is complete
aws --profile default --region us-west-2 \
 cloudformation wait stack-create-complete --stack-name user-eltest
```

#### Deploying directly

if you do not want to bother with ClodFormation, you can create a standalone function via:
```
aws lambda create-function \
 --function-name helloByol \
 --memory-size 1024 \
 --handler eltest \
 --zip-file fileb://_build/prod/eltest-0.0.0.zip \ 
 --runtime provided \
 --region us-west-2 \ 
 --role <role-arn>
```

#### Running and invoking your Erlang Lamdba function

With the Lambda function now deployed into your development account, go into
the AWS Lambda console, find your function, and hit Test button (accepting the
default test event document for now).  

You can also invoke your laptop
```
aws --profile default --region us-west-2 \
 lambda invoke  --function-name us-west-2-user-eltest \
  --log-type Tail \
  --payload '{"msg": "hello"}' \
  outputfile.txt
```

This should report that `"eltest:completed successfully"` and the CloudWatch Logs should show something like
this:

```
creating necessary erllambda run dirs
OpenSSL is OpenSSL 1.0.1k-fips 8 Jan 2015
starting ErlangVM
21:26:26.822005
Exec: /var/task/erts-9.3.3.3/bin/erlexec -noshell -noinput -Bd -boot /var/task/releases/0.0.0/eltest -mode embedded -boot_var ERTS_LIB_DIR /var/task/erts-9.3.3.3/lib -config /tmp/erllambda_rundir/sys.config -args_file /tmp/erllambda_rundir/vm.args --
Root: /var/task
[{supervisor,{local,erllambda_sup}},{started,[{pid,<0.883.0>},{id,erllambda_error_handler},{mfargs,{erllambda_error_handler,start_link,[]}},{restart_type,permanent},{shutdown,15000},{child_type,worker}]}]
[{supervisor,{local,erllambda_sup}},{started,[{pid,<0.884.0>},{id,erllambda_config_srv},{mfargs,{erllambda_config_srv,start_link,[]}},{restart_type,permanent},{shutdown,15000},{child_type,worker}]}]
[{application,erllambda},{started_at,nonode@nohost}]
[{application,eltest},{started_at,nonode@nohost}]
Invoke Next path 1542749188444 http://127.0.0.1:9001/2018-06-01/runtime/invocation/next
START RequestId: f008e2a2-ed0a-11e8-8771-4d0719f73d5f Version: $LATEST
[context@36312 aid="f008e2a2-ed0a-11e8-8771-4d0719f73d5f"] 127.0.0.1 - - [20/Nov/2018:21:26:28 -0000] Invoke Next
Next returns, in invoke 1542749188446
Hello World!
Invoke Success path 1542749188447 http://127.0.0.1:9001/2018-06-01/runtime/invocation/f008e2a2-ed0a-11e8-8771-4d0719f73d5f/response
Invoke Next path 1542749188449 http://127.0.0.1:9001/2018-06-01/runtime/invocation/next
END RequestId: f008e2a2-ed0a-11e8-8771-4d0719f73d5f
REPORT RequestId: f008e2a2-ed0a-11e8-8771-4d0719f73d5f	Init Duration: 1645.22 ms	Duration: 3.91 ms	Billed Duration: 1700 ms Memory Size: 2048 MB	Max Memory Used: 114 MB	
```

That's it! You new have an AWS Lambda function running in Erlang.

### Updating Lambda code 

To update your AWS Lambda after you made some changes to you code do the following
```
rebar3 compile
rebar3 erllambda release
rebar3 erllambda zip

```
update the function directly from your machine (for exact filename see `git describe` or `_build/prod` folder)

```
aws --profile default --region us-west-2 \
 lambda update-function-code \
 --function-name us-west-2-user-eltest \
 --zip-file fileb://_build/prod/eltest-0.0.0-1-g6a8495c.zip
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

For complex changes, or the introduction of a major feature, it is
beneficial to discuss ideas before implementing them, so that your efforts
can focus on pull requests that will be accepted more easily.

As you prepare your pull request, make sure that you follow the coding
conventions that exist in the files, and always make sure that all unit and
common tests run.  Please ensure that your contribution always adds to the
coverage percentage, and does not decrease it.


## How to report defects

If you encounter an problem, or simply have a question about using this
repo, please submit a
[github issue](https://github.com/alertlogic/rebar3_erllambda/issues).


## Initial setup, compilation and testing

*TLDR;* as long as your basic environment is setup, getting started
developing `rebar3_erllambda` should be as easy as forking the repo, and then:

```
git clone git@github.com:${USER}/rebar3_erllambda.git
cd rebar3_erllambda
git remote add upstream git@agithub.com:alertlogic/rebar3_erllambda.git
rebar3 get-deps compile test
```

<!--- vim: sw=4 et ts=4 -->
