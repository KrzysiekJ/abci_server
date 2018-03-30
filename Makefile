PROJECT = abci_server
PROJECT_DESCRIPTION = An Application Blockchain Interface server
PROJECT_VERSION = 0.5.0

DEPS = gpb ranch
dep_gpb = git https://github.com/tomas-abrahamsson/gpb 4.0.2
dep_ranch = git https://github.com/ninenines/ranch 1.3.2

TEST_DEPS = triq
dep_triq = git https://gitlab.com/triq/triq.git 79bd272025434e152745067c36350faa7274c653

# Whitespace to be used when creating files from templates.
SP = 4

include erlang.mk

.PHONY: gpb
gpb: deps
	./deps/gpb/bin/protoc-erl -o-erl src/ -o-hrl include/ -type -il -I include/ abci.proto
	sed -i 's/-include("abci.hrl")\./-include_lib("include\/abci.hrl")./' src/abci.erl
