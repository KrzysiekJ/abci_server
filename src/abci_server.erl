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
-ifdef(TEST).
-include_lib("triq/include/triq.hrl").
-include_lib("eunit/include/eunit.hrl").
-endif.

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

-spec decode_varint(binary()) -> none | {non_neg_integer(), binary()}.
decode_varint(<<0:1, Int:7, Rest/binary>>) ->
    {Int, Rest};
decode_varint(<<1:1, Int:7, TailBin/binary>>) ->
    case decode_varint(TailBin) of
        none ->
            none;
        {TailInt, RestBin} ->
            {Int + (TailInt bsl 7), RestBin}
    end;
decode_varint(_) ->
    none.

-spec decode_zigzag(binary()) -> none | {integer(), binary()}.
decode_zigzag(Bin) ->
    case decode_varint(Bin) of
        none ->
            none;
        {VarInt, RestBin} ->
            Int =
                case VarInt band 1 of
                    1 ->
                        -((VarInt + 1) bsr 1);
                    0 ->
                        VarInt bsr 1
                end,
            {Int, RestBin}
    end.

-spec unpack_requests(binary()) -> {list(abci:'tendermint.abci.types.Request'()), Rest :: binary()}.
unpack_requests(<<>>) ->
    {[], <<>>};
unpack_requests(DataBin) ->
    case decode_zigzag(DataBin) of
        none ->
            {[], DataBin};
        {Length, RestBin} ->
            case RestBin of
                <<Msg:Length/binary, RestBin2/binary>> ->
                    Request = abci:decode_msg(Msg, 'tendermint.abci.types.Request'),
                    {RestRequests, RestBin3} = unpack_requests(RestBin2),
                    {[Request|RestRequests], RestBin3};
                _ ->
                    {[], DataBin}
            end
    end.

-spec handle_requests(list(abci:'tendermint.abci.types.Request'()), #state{}) -> #state{}.
handle_requests([#'tendermint.abci.types.Request'{value={MsgName, RequestValue}}|RestRequests], State=#state{callback_mod=CallbackMod}) ->
    %% io:format("Received request ~w~n", [RequestValue]),
    ResponseValue =
        case RequestValue of
            #'tendermint.abci.types.RequestFlush'{} ->
                #'tendermint.abci.types.ResponseFlush'{};
            #'tendermint.abci.types.RequestEcho'{message=Message} ->
                #'tendermint.abci.types.ResponseEcho'{message=Message};
            _ ->
                CallbackMod:handle_request(RequestValue)
        end,
    ok = send_response({MsgName, ResponseValue}, State),
    handle_requests(RestRequests, State);
handle_requests([], State) ->
    State.

-spec send_response(any(), #state{}) -> ok.
send_response(ResponseValue, #state{socket=Socket, transport=Transport}) ->
    EncodedResponse = abci:encode_msg(#'tendermint.abci.types.Response'{value=ResponseValue}),
    %% io:format("Response: ~w~n", [EncodedResponse]),
    EncodedLength = encode_zigzag(byte_size(EncodedResponse)),
    FullResponse = <<EncodedLength/binary, EncodedResponse/binary>>,
    _ = Transport:setopts(Socket, [{active, once}]),
    ok = Transport:send(Socket, FullResponse),
    ok.

-spec encode_varint(non_neg_integer()) -> binary().
encode_varint(Int) ->
    encode_varint(Int, <<>>).

-spec encode_varint(non_neg_integer(), binary()) -> binary().
encode_varint(Int, Acc) ->
    Group = Int rem 128,
    Rest = Int bsr 7,
    case Rest of
        0 ->
            <<Acc/binary, 0:1, Group:7>>;
        _ ->
            encode_varint(Rest, <<Acc/binary, 1:1, Group:7>>)
    end.

-spec encode_zigzag(integer()) -> binary().
encode_zigzag(Int) ->
    ZigzagInt =
        case Int >= 0 of
            true ->
                2 * Int;
            %% Dialyzer here correctly points that we won’t ever encode negative lengths.
            %% Why therefore does ABCI use zigzag encoding for lengths instead of ordinary varint?
            false ->
                -2 * Int - 1
        end,
    encode_varint(ZigzagInt).

-dialyzer({no_match, encode_zigzag/1}).

-ifdef(TEST).
unpack_requests_test_() ->
    [?_assertEqual(
        {[#'tendermint.abci.types.Request'{value={info, #'tendermint.abci.types.RequestInfo'{}}},
          #'tendermint.abci.types.Request'{value={flush, #'tendermint.abci.types.RequestFlush'{}}}],
         <<>>},
        unpack_requests(<<2#000000100,34,0,2#00000100,26,0>>)),
     ?_assertEqual(
        {[#'tendermint.abci.types.Request'{value={info, #'tendermint.abci.types.RequestInfo'{}}},
          #'tendermint.abci.types.Request'{value={flush, #'tendermint.abci.types.RequestFlush'{}}}],
         <<10,11>>},
        unpack_requests(<<2#00000100,34,0,2#00000100,26,0,10,11>>))].

encode_zigzag_test_() ->
    [?_assertEqual(
        <<2#10101100, 2#00000010>>,
        encode_zigzag(150)),
     ?_assertEqual(
        <<2#10101011, 2#00000010>>,
        encode_zigzag(-150))].

prop_zigzag_encode_decode() ->
    ?FORALL(
       I,
       int(),
       {I, <<>>} =:= decode_zigzag(encode_zigzag(I))).
-endif.
