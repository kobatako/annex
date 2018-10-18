%%%-------------------------------------------------------------------
%% @doc annex proxy public API
%% @end
%%%-------------------------------------------------------------------
-module(annex_proxy).

-export([start_link/2]).
-export([accept/2]).

-export([init/1]).
-export([ip_to_binary/1]).

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
  {ok, ListenSocket} = gen_tcp:listen(Receive,
      [binary, {active, false}, {reuseaddr, true}, {backlog, 64}, inet]
  ),
  spawn_link(?MODULE, accept, [ListenSocket, Destination]),
  {ok, []}.

%%--------------------------------------------------------------------
%
% accept
%
-spec accept(port(), list()) -> pid().
accept(ListenSocket, [{host, Host}, {port, Port}]=Destination) ->
  case gen_tcp:accept(ListenSocket) of
    {ok, Socket} ->
      {ok, Con} = gen_tcp:connect(Host, Port, [binary, {packet, 0}]),
      inet:setopts(Socket, [{active, once}]),
      loop(Socket, Con);
    {error, closed} ->
      ok;
    {error, Reson} ->
      false
  end,
  spawn(?MODULE, accept, [ListenSocket, Destination]).


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
      gen_tcp:close(Front);

    {tcp_error, Front, Reson} ->
      false;

    {tcp, Back, Message} ->
      gen_tcp:send(Front, Message),
      loop(Front, Back);

    {tcp_closed, Back} ->
      gen_tcp:close(Back);

    {tcp_error, Back, Reson} ->
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

