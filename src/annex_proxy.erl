%%%-------------------------------------------------------------------
%% @doc annex proxy public API
%% @end
%%%-------------------------------------------------------------------

-module(annex_proxy).

-export([start_link/2]).
-export([accept/2]).

-export([init/1]).

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
      [binary, {active, false}, {reuseaddr, true}, {backlog, 64}]
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
      io:format("fail accept ~p~n", [Reson])
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
      io:format("~p~n", [Message]),
      Parse = annex_http:parse(Message),
      io:format("parse : ~p~n", [Parse]),
      gen_tcp:send(Back, Message),
      loop(Front, Back);

    {tcp_closed, Front} ->
      gen_tcp:close(Front);

    {tcp_error, Front, Reson} ->
      io:format("Handle error ~p on ~p~n", [Reson, Front]);

    {tcp, Back, Message} ->
      gen_tcp:send(Front, Message),
      loop(Front, Back);

    {tcp_closed, Back} ->
      gen_tcp:close(Back);

    {tcp_error, Back, Reson} ->
      io:format("Handle error ~p on ~p~n", [Reson, Back])
  end.

