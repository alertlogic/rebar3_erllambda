rebar3_erllambda
===========

Enable AWS Lambda function to be written in Erlang


## Overview

The `rebar3_erllambda` implements a
[Rebar3 Plugin](http://www.rebar3.org/docs/plugins) to facilitate the
development of [AWS Lambda](https://aws.amazon.com/lambda/)
functions, written entirely in Erlang.  This plugin works in concert with
the [`erllambda`](https://algithub.pd.alertlogic.net/alertlogic/erllambda)
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
two profiles defined in your `~/.aws/credentials` file: `default` and
`integration`.  This should look like the following:

```
[cd15_master]
aws_access_key_id = AKIAJ...............
aws_secret_access_key = 29b....................................

[default]
role_arn=arn:aws:iam::DEV_ACCOUNT_ID:role/centralized-users
source_profile=cd15_master

[integration]
role_arn=arn:aws:iam::948063967832:role/centralized-users
source_profile=cd15_master
```

Obviously, this needs read ID/SECRET information, and the development
account that your team uses.


Finally, the `rebar3_erllambda` plugin comes with a complete
[rebar3 template](http://www.rebar3.org/docs/using-templates) for getting
started with a running AWS lambda skeleton. You will need to add the plugin
to the global rebar3 environment.  To do this add the following to the
`$HOME/.config/rebar3/rebar.config` file, which will make the plugin
globally available:

```
{plugins,
 [
  {rebar3_erllambda,
   {git, "algithub.pd.alertlogic.net:alertlogic/rebar3_erllambda.git",
    {branch, master}}}
 ]}.
```

**NOTE:** Using the plugin globally does not appear to work for template
generation as of `rebar3` version 3.3.5.  To work around this, you will need
to create a bootstrap `rebar.config` file in your project directory.
Hopefully, this will get resolved in `rebar3` and then this step will not be
necessary.


### Bootstraping Your Environment (Mac OS)

If you are working on Mac OS, in addition to the instructions above, you
will want to leverage the
[docker-image-makeincl](https://algithub.pd.alertlogic.net/alertlogic/docker-image-makeincl)
project. to make development as simple as working directly on Linux.  To do
so, follow the
[Initial Setup](https://algithub.pd.alertlogic.net/alertlogic/docker-image-makeincl/blob/master/README.md#initial-setup)
instructions to get docker-machine working on your development system.  This
will enable you to work natively from the command-line and even deploy your
Lambda function directly from the Mac without any further build
infrastructure.


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
   {git, "algithub.pd.alertlogic.net:alertlogic/rebar3_erllambda.git",
    {branch, master}}}
 ]}.
```

Once you have your project directory created, then you want to ask the
plugin to generate the skeleton files for the project:

```
$ rebar3 new -f name=eltest
===> Fetching rebar3_erllambda ({git,
                                        "algithub.pd.alertlogic.net:alertlogic/rebar3_erllambda.git",
                                        {branch,master}})
===> Compiling rebar3_erllambda
===> Writing .edts
===> Writing .gitignore
===> Writing setup.sh
===> Writing setup/setenv
===> Writing README.md
===> Writing makefile
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
make env
. .setenv
rebar3 new -f erllambda name=eltest
git init
git add -A
git commit -m "Initial Skeleton"
git tag -m "Initial Skeleton" -a 0.0.0
```

Now you are ready to do your first build and deploy.  To do so, simply
execute the following:

```
$ dsh make stack-create
Starting with: pfisher(501)
executing: make stack-create
verifying templates: /home/pfisher/src/test/eltest/etc/eltest.template
Building release (profile=prod, devmode=false)
===> Compiling rebar3_erllambda
===> Verifying dependencies...
...
===> Compiling cowboy
===> Compiling erllambda
===> Compiling eltest
===> Starting relx build process ...
===> Resolving OTP Applications from directories:
          /home/pfisher/src/test/eltest/_build/prod/lib
          /home/pfisher/src/test/eltest/_checkouts
          /usr/local/lib/erlang/r19_2/lib
===> Resolved eltest-0.0.0
===> Including Erts from /usr/local/lib/erlang/r19_2
===> release successfully created!
===> running erllambda release generator
===> generating erllambda npm install
===> generating start script bin/bin/eltest
===> generating config file etc/handler.json
Generating zip file from release
rebar3 as prod erllambda zip
===> Compiling rebar3_erllambda
===> Compiling rebar3_erllambda
===> Compiling rebar3_erllambda
===> generating erllambda zip package
===> executing: /home/pfisher/src/test/eltest/_build/prod/lib/erllambda/priv/lambda-zip /home/pfisher/src/test/eltest/_build/prod/eltest-0.0.0.zip /home/pfisher/src/test/eltest/_build/prod/rel/eltest
successfully built _build/prod/eltest-0.0.0.zip
==> Finished uploading global artifacts
upload: _build/prod/eltest-0.0.0.zip to s3://us-west-2.route105.repository/pfisher/eltest/eltest-0.0.0.zip
upload: _build/artifacts/eltest-0.0.0.template to s3://us-west-2.route105.repository/pfisher/eltest/eltest-0.0.0.template
upload: _build/prod/eltest-0.0.0.zip to s3://us-east-1.route105.repository/pfisher/eltest/eltest-0.0.0.zip
upload: _build/artifacts/eltest-0.0.0.template to s3://us-east-1.route105.repository/pfisher/eltest/eltest-0.0.0.template
upload: _build/prod/eltest-0.0.0.zip to s3://eu-west-1.route105.repository/pfisher/eltest/eltest-0.0.0.zip
upload: _build/artifacts/eltest-0.0.0.template to s3://eu-west-1.route105.repository/pfisher/eltest/eltest-0.0.0.template
==> Finished uploading regional artifacts
===> Artifact upload successful
Creating stack pfisher-eltest...
{
    "StackId": "arn:aws:cloudformation:us-west-2:468472542084:stack/pfisher-eltest/6bd7c1a0-ebc7-11e6-9c4c-50d5ca2e7cd2"
}
Waiting for stack create to complete...
Done!
```

With the Lambda function now deployed into your development account, go into
the AWS Lambda console, find your function, and hit test (accepting the
default test event document for now).  This should report that `"eltest:
completed successfully"` and the CloudWatch Logs should show something like
this:

```
START RequestId: b0867638-ebc7-11e6-adad-5bbe0f75240a Version: $LATEST
2017-02-05T17:22:38.284Z	b0867638-ebc7-11e6-adad-5bbe0f75240a	linking /tmp/tmp-1C79BYOT2PJPu/vm.args -> /var/task/releases/0.0.0/vm.args
2017-02-05T17:22:38.285Z	b0867638-ebc7-11e6-adad-5bbe0f75240a	linking /tmp/tmp-1C79BYOT2PJPu/sys.config -> /var/task/releases/0.0.0/sys.config
2017-02-05T17:22:38.285Z	b0867638-ebc7-11e6-adad-5bbe0f75240a	creating dir: /tmp/tmp-1C79BYOT2PJPu/cachefs
2017-02-05T17:22:38.285Z	b0867638-ebc7-11e6-adad-5bbe0f75240a	creating dir: /tmp/tmp-1C79BYOT2PJPu/checkpointfs
2017-02-05T17:22:38.302Z	b0867638-ebc7-11e6-adad-5bbe0f75240a	creating dir: /tmp/tmp-1C79BYOT2PJPu/tmpfs
2017-02-05T17:22:38.302Z	b0867638-ebc7-11e6-adad-5bbe0f75240a	creating dir: /tmp/tmp-1C79BYOT2PJPu/ramfs
2017-02-05T17:22:38.302Z	b0867638-ebc7-11e6-adad-5bbe0f75240a	executing: "bin/eltest" with env:
{
    "AWS_ACCESS_KEY_ID": "ASIAICJMCO74OEQFMPQQ",
    "AWS_SECRET_ACCESS_KEY": "PWrNAr9kBZn+DhKOHsLkaJz5CeYKjMPfHo1grrK8",
    "AWS_SECURITY_TOKEN": "FQoDYXdzEBsaDFibkE8Yq3fRYx4cdSLxAZ9NBkafqA/8H090mylTMr35vr1gAKFnb35Pce6PQgFfwopKEgr5cNMCqlaej7xIfvnUhCx2oEfdRPQMGuFtnSPtMSaji6/mB0M6ToegKStdQhKaF3GsmEs+v0DqvALKBYRV7MdG9IqU8MmnbGulSLNjxd+HH9Tp+9z3/pktqJIZv8015475uNveH2E5B6dBtT/LlsSyNXHBrYJOrQht7XfNmiWnWq2FDLSEdkX2XdazUMFDZF8I57Z1Fgfx8V1gMg6cVUJTtj+nqbRF+LQsdM+om2FCOv9q12CmD3/e4f1LtzhcSzHlx+DLWGPqTn+FmbIo3b7dxAU=",
    "NATIVELIB_DIR": "/var/task/erts-*/lib",
    "VAR_DIR": "/tmp/tmp-1C79BYOT2PJPu",
    "RUN_DIR": "/tmp/tmp-1C79BYOT2PJPu",
    "PROGNAME": "eltest"
}

2017-02-05T17:22:38.345Z	b0867638-ebc7-11e6-adad-5bbe0f75240a	Exec: /var/task/erts-8.2/bin/erlexec -noshell -noinput -Bd -boot /var/task/releases/0.0.0/eltest -mode embedded -boot_var ERTS_LIB_DIR /var/task/erts-8.2/../lib -config /var/task/releases/0.0.0/sys.config -args_file /var/task/releases/0.0.0/vm.args --
Root: /var/task

2017-02-05T17:22:41.425Z	b0867638-ebc7-11e6-adad-5bbe0f75240a	erlang alive: success 100
2017-02-05T17:22:41.444Z	b0867638-ebc7-11e6-adad-5bbe0f75240a	
=INFO REPORT==== 5-Feb-2017::17:22:41 ===
eltest: Hello World!
END RequestId: b0867638-ebc7-11e6-adad-5bbe0f75240a
REPORT RequestId: b0867638-ebc7-11e6-adad-5bbe0f75240a	Duration: 3209.74 ms	Billed Duration: 3300 ms Memory Size: 512 MB	Max Memory Used: 66 MB
```

That's it! You new have an AWS Lambda function running in Erlang.


## Ownership

The `rebar3_erllambda` application owned by the
[Data Processing Team](https://alertlogic.atlassian.net/wiki/display/DPT).


## Dependencies

The `rebar3_erllambda` application is built using
[`rebar3`](http://www.rebar3.org), and all other dependencies are
automatically pulled in when `rebar3_erllambda` is used in other projects
`rebar.config`.

In addition makefile support is available in
[makeincl](https://algithub.pd.alertlogic.net/alertlogic/makeincl) with
makes builds and pipeline integration trivial.


## How to contribute

Contributions to this repo are always welcome.  If you have an idea for
improving the this or related components, please submit a
[github issue](https://algithub.pd.alertlogic.net/alertlogic/rebar3_erllambda/issues),
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
[github issue](https://algithub.pd.alertlogic.net/alertlogic/rebar3_erllambda/issues).


## Initial setup, compilation and testing

*TLDR;* as long as your basic environment is setup, getting started
developing `rebar3_erllambda` should be as easy as forking the repo, and then:

```
git clone git@algithub.pd.alertlogic.net:${USER}/rebar3_erllambda.git
cd rebar3_erllambda
git remote add upstream git@algithub.pd.alertlogic.net:alertlogic/rebar3_erllambda.git
make env
. .setenv
make deps compile test
```

## Makefile Targets

=======
After initial setup, and in future shell sessions, only do the following is
needed configure the environment for developement:

```sh
cd rebar3_erllambda
. .setenv
```

The main `makefile` targets for development and test are as follows:

- `make` will compile, execute the eunit tests, and generate a coverage
  report.
- `make unit` same as plain `make`
- `make ct` will compile, execute the common tests, and generate a coverage
  report.
- `make test` will compile, execute both the eunit and common tests, and
  generate a consolidated coverage report.

Full documentation of the makefile targets available and how to customize
`allib` makefiles can be found in
[makeincl](https://algithub.pd.alertlogic.net/alertlogic/makeincl).


<!--- vim: sw=4 et ts=4 -->
