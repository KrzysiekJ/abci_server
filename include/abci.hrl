%% -*- coding: utf-8 -*-
%% Automatically generated, do not edit
%% Generated by gpb_compile version 3.26.8

-ifndef(abci).
-define(abci, true).

-define(abci_gpb_version, "3.26.8").

-ifndef('REQUESTINFO_PB_H').
-define('REQUESTINFO_PB_H', true).
-record('RequestInfo',
        {
        }).
-endif.

-ifndef('RESPONSEINITCHAIN_PB_H').
-define('RESPONSEINITCHAIN_PB_H', true).
-record('ResponseInitChain',
        {
        }).
-endif.

-ifndef('PARTSETHEADER_PB_H').
-define('PARTSETHEADER_PB_H', true).
-record('PartSetHeader',
        {total                  :: non_neg_integer() | undefined, % = 1, 32 bits
         hash                   :: binary() | undefined % = 2
        }).
-endif.

-ifndef('BLOCKID_PB_H').
-define('BLOCKID_PB_H', true).
-record('BlockID',
        {hash                   :: binary() | undefined, % = 1
         parts                  :: #'PartSetHeader'{} | undefined % = 2
        }).
-endif.

-ifndef('HEADER_PB_H').
-define('HEADER_PB_H', true).
-record('Header',
        {chain_id               :: iolist() | undefined, % = 1
         height                 :: non_neg_integer() | undefined, % = 2, 32 bits
         time                   :: non_neg_integer() | undefined, % = 3, 32 bits
         num_txs                :: non_neg_integer() | undefined, % = 4, 32 bits
         last_block_id          :: #'BlockID'{} | undefined, % = 5
         last_commit_hash       :: binary() | undefined, % = 6
         data_hash              :: binary() | undefined, % = 7
         validators_hash        :: binary() | undefined, % = 8
         app_hash               :: binary() | undefined % = 9
        }).
-endif.

-ifndef('REQUESTBEGINBLOCK_PB_H').
-define('REQUESTBEGINBLOCK_PB_H', true).
-record('RequestBeginBlock',
        {hash                   :: binary() | undefined, % = 1
         header                 :: #'Header'{} | undefined % = 2
        }).
-endif.

-ifndef('VALIDATOR_PB_H').
-define('VALIDATOR_PB_H', true).
-record('Validator',
        {pubKey                 :: binary() | undefined, % = 1
         power                  :: non_neg_integer() | undefined % = 2, 32 bits
        }).
-endif.

-ifndef('RESPONSEENDBLOCK_PB_H').
-define('RESPONSEENDBLOCK_PB_H', true).
-record('ResponseEndBlock',
        {diffs = []             :: [#'Validator'{}] % = 1
        }).
-endif.

-ifndef('REQUESTDELIVERTX_PB_H').
-define('REQUESTDELIVERTX_PB_H', true).
-record('RequestDeliverTx',
        {tx                     :: binary() | undefined % = 1
        }).
-endif.

-ifndef('REQUESTCHECKTX_PB_H').
-define('REQUESTCHECKTX_PB_H', true).
-record('RequestCheckTx',
        {tx                     :: binary() | undefined % = 1
        }).
-endif.

-ifndef('REQUESTCOMMIT_PB_H').
-define('REQUESTCOMMIT_PB_H', true).
-record('RequestCommit',
        {
        }).
-endif.

-ifndef('REQUESTENDBLOCK_PB_H').
-define('REQUESTENDBLOCK_PB_H', true).
-record('RequestEndBlock',
        {height                 :: non_neg_integer() | undefined % = 1, 32 bits
        }).
-endif.

-ifndef('REQUESTINITCHAIN_PB_H').
-define('REQUESTINITCHAIN_PB_H', true).
-record('RequestInitChain',
        {validators = []        :: [#'Validator'{}] % = 1
        }).
-endif.

-ifndef('REQUESTQUERY_PB_H').
-define('REQUESTQUERY_PB_H', true).
-record('RequestQuery',
        {data                   :: binary() | undefined, % = 1
         path                   :: iolist() | undefined, % = 2
         height                 :: non_neg_integer() | undefined, % = 3, 32 bits
         prove                  :: boolean() | 0 | 1 | undefined % = 4
        }).
-endif.

-ifndef('REQUESTSETOPTION_PB_H').
-define('REQUESTSETOPTION_PB_H', true).
-record('RequestSetOption',
        {key                    :: iolist() | undefined, % = 1
         value                  :: iolist() | undefined % = 2
        }).
-endif.

-ifndef('REQUESTFLUSH_PB_H').
-define('REQUESTFLUSH_PB_H', true).
-record('RequestFlush',
        {
        }).
-endif.

-ifndef('REQUESTECHO_PB_H').
-define('REQUESTECHO_PB_H', true).
-record('RequestEcho',
        {message                :: iolist() | undefined % = 1
        }).
-endif.

-ifndef('REQUEST_PB_H').
-define('REQUEST_PB_H', true).
-record('Request',
        {value                  :: {echo, #'RequestEcho'{}} | {flush, #'RequestFlush'{}} | {info, #'RequestInfo'{}} | {set_option, #'RequestSetOption'{}} | {deliver_tx, #'RequestDeliverTx'{}} | {check_tx, #'RequestCheckTx'{}} | {commit, #'RequestCommit'{}} | {query, #'RequestQuery'{}} | {init_chain, #'RequestInitChain'{}} | {begin_block, #'RequestBeginBlock'{}} | {end_block, #'RequestEndBlock'{}} | undefined % oneof
        }).
-endif.

-ifndef('RESPONSEINFO_PB_H').
-define('RESPONSEINFO_PB_H', true).
-record('ResponseInfo',
        {data                   :: iolist() | undefined, % = 1
         version                :: iolist() | undefined, % = 2
         last_block_height      :: non_neg_integer() | undefined, % = 3, 32 bits
         last_block_app_hash    :: binary() | undefined % = 4
        }).
-endif.

-ifndef('RESPONSESETOPTION_PB_H').
-define('RESPONSESETOPTION_PB_H', true).
-record('ResponseSetOption',
        {log                    :: iolist() | undefined % = 1
        }).
-endif.

-ifndef('RESPONSECHECKTX_PB_H').
-define('RESPONSECHECKTX_PB_H', true).
-record('ResponseCheckTx',
        {code                   :: 'OK' | 'InternalError' | 'EncodingError' | 'BadNonce' | 'Unauthorized' | 'InsufficientFunds' | 'UnknownRequest' | 'BaseDuplicateAddress' | 'BaseEncodingError' | 'BaseInsufficientFees' | 'BaseInsufficientFunds' | 'BaseInsufficientGasPrice' | 'BaseInvalidInput' | 'BaseInvalidOutput' | 'BaseInvalidPubKey' | 'BaseInvalidSequence' | 'BaseInvalidSignature' | 'BaseUnknownAddress' | 'BaseUnknownPubKey' | 'BaseUnknownPlugin' | 'GovUnknownEntity' | 'GovUnknownGroup' | 'GovUnknownProposal' | 'GovDuplicateGroup' | 'GovDuplicateMember' | 'GovDuplicateProposal' | 'GovDuplicateVote' | 'GovInvalidMember' | 'GovInvalidVote' | 'GovInvalidVotingPower' | integer() | undefined, % = 1, enum CodeType
         data                   :: binary() | undefined, % = 2
         log                    :: iolist() | undefined % = 3
        }).
-endif.

-ifndef('RESPONSEDELIVERTX_PB_H').
-define('RESPONSEDELIVERTX_PB_H', true).
-record('ResponseDeliverTx',
        {code                   :: 'OK' | 'InternalError' | 'EncodingError' | 'BadNonce' | 'Unauthorized' | 'InsufficientFunds' | 'UnknownRequest' | 'BaseDuplicateAddress' | 'BaseEncodingError' | 'BaseInsufficientFees' | 'BaseInsufficientFunds' | 'BaseInsufficientGasPrice' | 'BaseInvalidInput' | 'BaseInvalidOutput' | 'BaseInvalidPubKey' | 'BaseInvalidSequence' | 'BaseInvalidSignature' | 'BaseUnknownAddress' | 'BaseUnknownPubKey' | 'BaseUnknownPlugin' | 'GovUnknownEntity' | 'GovUnknownGroup' | 'GovUnknownProposal' | 'GovDuplicateGroup' | 'GovDuplicateMember' | 'GovDuplicateProposal' | 'GovDuplicateVote' | 'GovInvalidMember' | 'GovInvalidVote' | 'GovInvalidVotingPower' | integer() | undefined, % = 1, enum CodeType
         data                   :: binary() | undefined, % = 2
         log                    :: iolist() | undefined % = 3
        }).
-endif.

-ifndef('RESPONSEBEGINBLOCK_PB_H').
-define('RESPONSEBEGINBLOCK_PB_H', true).
-record('ResponseBeginBlock',
        {
        }).
-endif.

-ifndef('RESPONSEQUERY_PB_H').
-define('RESPONSEQUERY_PB_H', true).
-record('ResponseQuery',
        {code                   :: 'OK' | 'InternalError' | 'EncodingError' | 'BadNonce' | 'Unauthorized' | 'InsufficientFunds' | 'UnknownRequest' | 'BaseDuplicateAddress' | 'BaseEncodingError' | 'BaseInsufficientFees' | 'BaseInsufficientFunds' | 'BaseInsufficientGasPrice' | 'BaseInvalidInput' | 'BaseInvalidOutput' | 'BaseInvalidPubKey' | 'BaseInvalidSequence' | 'BaseInvalidSignature' | 'BaseUnknownAddress' | 'BaseUnknownPubKey' | 'BaseUnknownPlugin' | 'GovUnknownEntity' | 'GovUnknownGroup' | 'GovUnknownProposal' | 'GovDuplicateGroup' | 'GovDuplicateMember' | 'GovDuplicateProposal' | 'GovDuplicateVote' | 'GovInvalidMember' | 'GovInvalidVote' | 'GovInvalidVotingPower' | integer() | undefined, % = 1, enum CodeType
         index                  :: integer() | undefined, % = 2, 32 bits
         key                    :: binary() | undefined, % = 3
         value                  :: binary() | undefined, % = 4
         proof                  :: binary() | undefined, % = 5
         height                 :: non_neg_integer() | undefined, % = 6, 32 bits
         log                    :: iolist() | undefined % = 7
        }).
-endif.

-ifndef('RESPONSEFLUSH_PB_H').
-define('RESPONSEFLUSH_PB_H', true).
-record('ResponseFlush',
        {
        }).
-endif.

-ifndef('RESPONSECOMMIT_PB_H').
-define('RESPONSECOMMIT_PB_H', true).
-record('ResponseCommit',
        {code                   :: 'OK' | 'InternalError' | 'EncodingError' | 'BadNonce' | 'Unauthorized' | 'InsufficientFunds' | 'UnknownRequest' | 'BaseDuplicateAddress' | 'BaseEncodingError' | 'BaseInsufficientFees' | 'BaseInsufficientFunds' | 'BaseInsufficientGasPrice' | 'BaseInvalidInput' | 'BaseInvalidOutput' | 'BaseInvalidPubKey' | 'BaseInvalidSequence' | 'BaseInvalidSignature' | 'BaseUnknownAddress' | 'BaseUnknownPubKey' | 'BaseUnknownPlugin' | 'GovUnknownEntity' | 'GovUnknownGroup' | 'GovUnknownProposal' | 'GovDuplicateGroup' | 'GovDuplicateMember' | 'GovDuplicateProposal' | 'GovDuplicateVote' | 'GovInvalidMember' | 'GovInvalidVote' | 'GovInvalidVotingPower' | integer() | undefined, % = 1, enum CodeType
         data                   :: binary() | undefined, % = 2
         log                    :: iolist() | undefined % = 3
        }).
-endif.

-ifndef('RESPONSEECHO_PB_H').
-define('RESPONSEECHO_PB_H', true).
-record('ResponseEcho',
        {message                :: iolist() | undefined % = 1
        }).
-endif.

-ifndef('RESPONSEEXCEPTION_PB_H').
-define('RESPONSEEXCEPTION_PB_H', true).
-record('ResponseException',
        {error                  :: iolist() | undefined % = 1
        }).
-endif.

-ifndef('RESPONSE_PB_H').
-define('RESPONSE_PB_H', true).
-record('Response',
        {value                  :: {exception, #'ResponseException'{}} | {echo, #'ResponseEcho'{}} | {flush, #'ResponseFlush'{}} | {info, #'ResponseInfo'{}} | {set_option, #'ResponseSetOption'{}} | {deliver_tx, #'ResponseDeliverTx'{}} | {check_tx, #'ResponseCheckTx'{}} | {commit, #'ResponseCommit'{}} | {query, #'ResponseQuery'{}} | {init_chain, #'ResponseInitChain'{}} | {begin_block, #'ResponseBeginBlock'{}} | {end_block, #'ResponseEndBlock'{}} | undefined % oneof
        }).
-endif.

-endif.