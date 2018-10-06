%%%-------------------------------------------------------------------
%% @doc annex proxy public API
%% @end
%%%-------------------------------------------------------------------

-module(annex_proxy).
-behaviour(gen_server).

-export([start_link/2]).
-export([accept/2]).
-export([proxy_accept/2]).

-export([init/1]).
-export([terminate/2]).
-export([handle_info/2]).
-export([handle_cast/2]).
-export([handle_call/3]).

%%====================================================================
%% API
%%====================================================================

%%--------------------------------------------------------------------
%
% start_link
%
-spec start_link(integer(), list()) -> {ok, pid()}.
start_link(Receive, Destination) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [Receive, Destination], []).

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
% proxy_accept
%
-spec proxy_accept(list(), pid()) -> any().
proxy_accept([{host, Host}, {port, Port}], FrontId) ->
  {ok, Con} = gen_tcp:connect(Host, Port, [binary, {packet, 0}]),
  loop(Con, FrontId).


%%--------------------------------------------------------------------
%
% accept
%
-spec accept(port(), list()) -> pid().
accept(ListenSocket, Destination) ->
  case gen_tcp:accept(ListenSocket) of
    {ok, Socket} ->
      BackId = spawn_link(?MODULE, proxy_accept, [Destination, self()]),
      loop(Socket, BackId);
    {error, closed} ->
      ok;
    {error, Reson} ->
      io:format("fail accept ~p~n", [Reson])
  end,
  spawn(?MODULE, accept, [ListenSocket, Destination]).


terminate(_Message, _Storage) ->
  ok.

handle_info(_Message, Storage) ->
  {noreplay, Storage}.

handle_call(?MODULE, _Message, Storage) ->
  {reply, ok, ok}.

handle_cast(_Message, Storage) ->
  {noreply, ok}.

%%====================================================================
%% Internal functions
%%====================================================================

%%--------------------------------------------------------------------
%
% loop
%
-spec loop(port(), atom()) -> any().
loop(Socket, SendPid) ->
  inet:setopts(Socket, [{active, once}]),

  receive
    {tcp, Socket, Message} ->
      SendPid ! {recv, Message},
      loop(Socket, SendPid);

    {tcp_closed, Socket} ->
      gen_tcp:close(Socket);

    {tcp_error, Socket, Reson} ->
      io:format("Handle error ~p on ~p~n", [Reson, Socket]);

    {recv, Message} ->
      gen_tcp:send(Socket, Message),
      loop(Socket, SendPid)
  end.

