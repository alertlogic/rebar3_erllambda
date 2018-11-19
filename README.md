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

If you are working on Mac OS, in addition to the instructions above, you
will want to leverage the
[erllambda_docker](https://github.com/alertlogic/erllambda_docker)
project. to make development as simple as working directly on Linux.  To do
so, follow the
[Initial Setup](https://github/alertlogic/docker-image-makeincl/blob/master/README.md#initial-setup)
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
...
2017-02-05T17:22:41.444Z	b0867638-ebc7-11e6-adad-5bbe0f75240a	eltest: Hello World! 
...
END RequestId: b0867638-ebc7-11e6-adad-5bbe0f75240a
REPORT RequestId: b0867638-ebc7-11e6-adad-5bbe0f75240a	Duration: 1000.00 ms	Billed Duration: 1000 ms Memory Size: 512 MB	Max Memory Used: 66 MB
```

That's it! You new have an AWS Lambda function running in Erlang.


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
