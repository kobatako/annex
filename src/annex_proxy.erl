%%%-------------------------------------------------------------------
%% @doc annex proxy public API
%% @end
%%%-------------------------------------------------------------------
-module(annex_proxy).

-export([init/1]).
-export([ip_to_binary/1]).
-export([start_link/2]).
-export([accept/2]).

%%====================================================================
%% API
%%====================================================================

%%--------------------------------------------------------------------
%
% start_link
%
-spec start_link(integer(), list()) -> {ok, pid()}.
start_link(Receive, Destination) ->
  init([Receive, Destination]).

%%--------------------------------------------------------------------
%
% init
%
-spec init(list()) -> {ok, list()}.
init([Receive, Destination]) ->
  {ok, ListenSocket} = ranch_tcp:listen([{port, Receive}]),
  Pid = spawn(?MODULE, accept, [ListenSocket, Destination]),
  {ok, self()}.

%%--------------------------------------------------------------------
%
% accept
%
-spec accept(port(), list()) -> pid().
accept(ListenSocket, [{host, Host}, {port, Port}]=Destination) ->
  case ranch_tcp:accept(ListenSocket, infinity) of
    {ok, Socket} ->
      {ok, Con} = gen_tcp:connect(Host, Port, [binary, {packet, 0}]),
      inet:setopts(Socket, [{active, once}]),
      spawn(?MODULE, accept, [ListenSocket, Destination]),
      loop(Socket, Con);
    {error, closed} ->
      ok;
    {error, _} ->
      false
  end.

%%--------------------------------------------------------------------
%
% ip_to_binary
%
-spec ip_to_binary(tuple()) -> bitstring().
ip_to_binary({I1, I2, I3, I4}) ->
  Acc = list_to_binary(integer_to_list(I1)),
  lists:foldl(fun(Oc, Ip) -> <<Ip/binary, ".", Oc/binary>> end, Acc,
    [list_to_binary(integer_to_list(I)) || I <- [I2, I3, I4]]).


%%====================================================================
%% Internal functions
%%====================================================================

%%--------------------------------------------------------------------
%
% loop
%
-spec loop(port(), port()) -> any().
loop(Front, Back) ->
  receive
    {tcp, Front, Message} ->
      Parse = annex_http:parse(Message),
      Send = annex_http:format(Parse, [
        annex_http:format_x_forwarded_for(fetch_source_ip(Front))
      ]),
      gen_tcp:send(Back, Send),
      loop(Front, Back);

    {tcp_closed, Front} ->
      ranch_tcp:close(Front),
      ranch_tcp:close(Back);

    {tcp_error, Front, _Reson} ->
      false;

    {tcp, Back, Message} ->
      gen_tcp:send(Front, Message),
      loop(Front, Back);

    {tcp_closed, Back} ->
      ranch_tcp:close(Front),
      ranch_tcp:close(Back);

    {tcp_error, Back, _Reson} ->
      false
  end.

%%--------------------------------------------------------------------
%
% fetch_source_ip
%
-spec fetch_source_ip(port()) -> bitstring().
fetch_source_ip(Socket) ->
  {ok, {Ip, _}} = inet:peername(Socket),
  ip_to_binary(Ip).

