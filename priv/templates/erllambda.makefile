APP := {{name}}

ERLANG_VERSION ?= 20.1.7
COMP_NEEDED := erlang_$(ERLANG_VERSION),rebar3,setenv
CFN_STACK_NAME := {{name}}
CFN_CAPABILITIES := CAPABILITY_NAMED_IAM

all: unit

env :
	@echo "Ensuring build environment is boostrapped..."
	@APP=$(APP) ERLANG_VERSION=$(ERLANG_VERSION) COMPS_NEEDED=$(COMP_NEEDED) ./setup.sh -u

MAKE_HOME = $(abspath $(or $(wildcard _checkouts/makeincl),\
			   $(wildcard _build/makeincl)))
-include $(MAKE_HOME)/makefile.erllambda
export MAKE_HOME

$(call set,STACK_PARAMS,baseStackName,$(ENVIRON))
$(call set,STACK_PARAMS,version,$(REV))
$(call set,STACK_PARAMS,artifactBucket,$(ART_REGIONAL_BUCKET))
