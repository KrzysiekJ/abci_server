PROJECT = abci_server
PROJECT_DESCRIPTION = An Application Blockchain Interface server
PROJECT_VERSION = 0.7.1

DEPS = ranch
dep_ranch = git https://github.com/ninenines/ranch 1.3.2

BUILD_DEPS = gpb
# Beware when upgrading, as gpb 4.3.2 uses GPL-licensed PropEr for testing.
# See https://github.com/tomas-abrahamsson/gpb/issues/152 for details.
dep_gpb = git https://github.com/tomas-abrahamsson/gpb 4.3.1

TEST_DEPS = triq
dep_triq = git https://gitlab.com/triq/triq.git 79bd272025434e152745067c36350faa7274c653

# Whitespace to be used when creating files from templates.
SP = 4

include erlang.mk

GPB_GENERATED_FILES = include/abci.hrl src/abci.erl

$(PROJECT).d:: $(GPB_GENERATED_FILES)

ABCI_PROTO_SOURCE = github.com/tendermint/tendermint/abci/types/types.proto

$(ABCI_PROTO_SOURCE):
	$(gen_verbose) git submodule update --init

include/abci.proto: $(ABCI_PROTO_SOURCE)
	$(gen_verbose) cp $(ABCI_PROTO_SOURCE) include/abci.proto
	$(gen_verbose) sed -i 's@package types;@package abci;@' include/abci.proto

$(GPB_GENERATED_FILES):: include/abci.proto
	$(gen_verbose) $(DEPS_DIR)/gpb/bin/protoc-erl -o-erl src/ -o-hrl include/ -type -I include/ -pkgs -pldefs include/abci.proto
	$(gen_verbose) sed -i 's@-include("abci.hrl")\.@-include_lib("include/abci.hrl").@' src/abci.erl

clean::
	$(gen_verbose) rm -f $(GPB_GENERATED_FILES) include/abci.proto

distclean::
	$(gen_verbose) git submodule deinit --all
