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

-module(abci_server).
-behaviour(gen_server).
-behaviour(ranch_protocol).

-include_lib("include/abci.hrl").

%% API.
-export([start_link/4]).
-export([child_spec/2,
         start_listener/2,
         stop_listener/1]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-record(
   state,
   {
     socket,
     transport,
     buffered= <<>>,
     callback_mod :: module()
   }).

%% API.

start_link(Ref, Socket, Transport, Opts) ->
    {ok, proc_lib:spawn_link(?MODULE, init, [{Ref, Socket, Transport, Opts}])}.

-spec ranch_args(CallbackMod :: module(), Port :: pos_integer()) -> Args :: term().
ranch_args(CallbackMod, Port) ->
    [CallbackMod,
     3,
     ranch_tcp,
     [{port, Port}, {max_connections, 3}],
     abci_server,
     [CallbackMod]].

-spec start_listener(CallbackMod :: module(), Port :: pos_integer()) -> supervisor:startchild_ret().
start_listener(CallbackMod, Port) ->
    apply(ranch, start_listener, ranch_args(CallbackMod, Port)).

-spec child_spec(CallbackMod :: module(), Port :: pos_integer()) -> supervisor:child_spec().
child_spec(CallbackMod, Port) ->
    apply(ranch, child_spec, ranch_args(CallbackMod, Port)).

-spec stop_listener(CallbackMod :: module()) -> ok | {error, any()}.
stop_listener(CallbackMod) ->
    ranch:stop_listener(CallbackMod).

%% gen_server.

init({Ref, Socket, Transport, [CallbackMod]}) ->
    ok = ranch:accept_ack(Ref),
    ok = Transport:setopts(Socket, [{active, once}]),
    gen_server:enter_loop(?MODULE, [], #state{socket=Socket, transport=Transport, callback_mod=CallbackMod}).

handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({tcp, Socket, Data}, State=#state{socket=Socket, buffered=Buffered}) ->
    {Requests, Rest} = unpack_requests(<<Buffered/binary, Data/binary>>),
    NewState = handle_requests(Requests, State#state{buffered=Rest}),
    {noreply, NewState};
handle_info({tcp_closed, Socket}, State=#state{socket=Socket}) ->
    {stop, normal, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

-spec unpack_requests(binary()) -> {list(abci:'Request'()), Rest :: binary()}.
unpack_requests(Data) ->
    case Data of
        <<LengthLength, Length:LengthLength/unit:8, Msg:Length/binary, Rest1/binary>> ->
            Request = abci:decode_msg(Msg, 'Request'),
            {RestRequests, Rest2} = unpack_requests(Rest1),
            {[Request|RestRequests], Rest2};
        _ ->
            {[], Data}
    end.

-spec handle_requests(list(abci:'Request'()), #state{}) -> #state{}.
handle_requests([#'Request'{value={MsgName, RequestValue}}|RestRequests], State=#state{callback_mod=CallbackMod}) ->
    %% io:format("Received request ~w~n", [RequestValue]),
    ResponseValue =
        case RequestValue of
            #'RequestFlush'{} ->
                #'ResponseFlush'{};
            #'RequestEcho'{message=Message} ->
                #'ResponseEcho'{message=Message};
            _ ->
                CallbackMod:handle_request(RequestValue)
        end,
    ok = send_response({MsgName, ResponseValue}, State),
    handle_requests(RestRequests, State);
handle_requests([], State) ->
    State.

-spec send_response(any(), #state{}) -> ok.
send_response(ResponseValue, #state{socket=Socket, transport=Transport}) ->
    EncodedResponse = abci:encode_msg(#'Response'{value=ResponseValue}),
    %% io:format("Response: ~w~n", [EncodedResponse]),
    EncodedLength = encode_length(byte_size(EncodedResponse)),
    FullResponse = <<EncodedLength/binary, EncodedResponse/binary>>,
    _ = Transport:setopts(Socket, [{active, once}]),
    ok = Transport:send(Socket, FullResponse),
    ok.

-spec encode_length(integer()) -> binary().
encode_length(Length) ->
    LengthBin = binary:encode_unsigned(Length),
    LengthSize = byte_size(LengthBin),
    <<LengthSize, LengthBin:LengthSize/binary>>.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

unpack_requests_test_() ->
    [?_assertEqual(
        {[#'Request'{value={info, #'RequestInfo'{}}},
          #'Request'{value={flush, #'RequestFlush'{}}}],
         <<>>},
        unpack_requests(<<1,2,26,0,1,2,18,0>>)),
     ?_assertEqual(
        {[#'Request'{value={info, #'RequestInfo'{}}},
          #'Request'{value={flush, #'RequestFlush'{}}}],
         <<10,11>>},
        unpack_requests(<<1,2,26,0,1,2,18,0,10,11>>))].

encode_length_test_() ->
    [?_assertEqual(
        <<1, 1>>,
        encode_length(1)),
     ?_assertEqual(
        <<1, 3>>,
        encode_length(3)),
     ?_assertEqual(
        <<1, 255>>,
        encode_length(255)),
     ?_assertEqual(
        <<2, 1, 0>>,
        encode_length(256)),
     ?_assertEqual(
        <<3, 1, 0, 0>>,
        encode_length(65536)
       )].
-endif.
