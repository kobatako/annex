-module(annex_proxy).
-behaviour(ranch_protocol).

-export([new_proxy/2]).

%%--------------------------------------------------------------------
%
% new proxy
%
-spec new_proxy(integer(), list()) -> ok.
new_proxy(Receive, Destination) ->
  Pid = spawn(fun() ->
      listen(Receive, Destination),
      timer:sleep(infinity)
  end),
  ok.

%%--------------------------------------------------------------------
%
% new proxy
%
-spec listen(integer(), list()) -> ok.
listen(Receive, Destination) ->
    {ok, ListenSocket} = gen_tcp:listen(Receive,
      [binary, {active, false}, {reuseaddr, true}, {backlog, 64}]
    ),
    spawn(fun() ->
      accept(ListenSocket, Destination)
    end).

%%--------------------------------------------------------------------
%
% accept
%
-spec accept(port(), list()) -> any().
accept(ListenSocket, [{host, Host}, {port,  Port}]=Destination) ->
  {ok, Socket} = gen_tcp:accept(ListenSocket),
  spawn(fun() ->
    accept(ListenSocket, Destination)
  end),
  {ok, Con} = gen_tcp:connect(Host, Port, [binary, {packet, 0}]),
  loop(Socket, Con).

%%--------------------------------------------------------------------
%
% loop
%
-spec loop(port(), port()) -> any().
loop(Socket, Con) ->
  io:format("socket ~p~n", [Socket]),
  io:format("con ~p~n", [Con]),
  inet:setopts(Socket, [{active, once}]),
  inet:setopts(Con, [{active, once}]),
  receive
    {tcp, Socket, Message} ->
      gen_tcp:send(Con, Message),
      loop(Socket, Con);
    {tcp_closed, Socket} ->
      io:format("close ~p~n", [Socket]),
      gen_tcp:close(Socket),
      gen_tcp:close(Con);
    {tcp_error, Socket, Reson} ->
      gen_tcp:close(Con),
      io:format("Handle error ~p on ~p~n", [Reson, Socket]);

    {tcp, Con, Message} ->
      gen_tcp:send(Socket, Message),
      loop(Socket, Con);
    {tcp_closed, Con} ->
      io:format("close ~p~n", [Con]),
      gen_tcp:close(Socket),
      gen_tcp:close(Con);
    {tcp_error, Con, Reson} ->
      gen_tcp:close(Socket),
      io:format("Handle error ~p on ~p~n", [Reson, Con])
  end.

