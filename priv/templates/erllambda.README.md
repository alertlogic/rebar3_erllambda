{{name}}
=================

An AWS Lambda function build using Erlang.


## Overview

The `{{name}}` is an [AWS Lambda](https://aws.amazon.com/lambda/)
function implemented using the [erllambda](https://github.com/alertlogic/erllambda)
application to implement the function using the Erlang language.

This project can be complied and deployed from your local checkout into an
AWS account in which you have sufficient credentials, and executed.
Instructions to do this can be found in
[Running this Project](#running-this-project).


## Ownership

The `{{name}}` application owned by the [Awesome Team]().


## Dependencies

The `{{name}}` application is built using
[`rebar3`](http://www.rebar3.org), and all other dependencies are
automatically pulled in when `{{name}}` is used in other projects
`rebar.config`.


## How to contribute

Contributions to this repo are always welcome.  If you have an idea for
improving the this or related components, please submit a
github issue or simply submit a PR directly that implements your improvement.

For complex changes, or the introduction of a major feature, it is
beneficial to discuss ideas before implementing them, so that your efforts
can focus on pull requests that will be accepted more easily.

As you prepare your pull request, make sure that you follow the coding
conventions that exist in the files, and always make sure that all unit and
common tests run.  Please ensure that your contribution always adds to the
coverage percentage, and does not decrease it.


## How to report defects

If you encounter an problem, or simply have a question about using this
repo, please submit a github issue.


## Running this Project

Initially, just clone and build the project.

```
git clone https://github.com/alertlogic/{{name}}.git
cd {{name}}
rebar3 get-deps compile ct
```

Once build and all tests successfully pass, you can package the lambda
function and deploy it.

Packaging and deployment process is documented [here](https://github.com/alertlogic/rebar3_erllambda#creating-a-package).

<!--- vim: sw=4 et ts=4 -->
