%% Licensed under the Apache License, Version 2.0 (the “License”);
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an “AS IS” BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

-module(abci_app).

-include_lib("include/abci.hrl").

-type request() :: abci:'abci.RequestInfo'() |
                   abci:'abci.RequestBeginBlock'() |
                   abci:'abci.RequestCheckTx'() |
                   abci:'abci.RequestCommit'() |
                   abci:'abci.RequestEndBlock'() |
                   abci:'abci.RequestInitChain'() |
                   abci:'abci.RequestQuery'() |
                   abci:'abci.RequestSetOption'().
-type response() :: abci:'abci.ResponseInitChain'() |
                    abci:'abci.ResponseEndBlock'() |
                    abci:'abci.ResponseInfo'() |
                    abci:'abci.ResponseSetOption'() |
                    abci:'abci.ResponseCheckTx'() |
                    abci:'abci.ResponseDeliverTx'() |
                    abci:'abci.ResponseBeginBlock'() |
                    abci:'abci.ResponseQuery'() |
                    abci:'abci.ResponseCommit'().

-export_type(
   [request/0,
    response/0]).

-callback handle_request(request()) -> response().
