%%%-------------------------------------------------------------------
%% @doc annex proxy public API
%% @end
%%%-------------------------------------------------------------------
-module(annex_proxy).

-export([init/1]).
-export([ip_to_binary/1]).
-export([start_link/2]).

%%====================================================================
%% API
%%====================================================================

%%--------------------------------------------------------------------
%
% start_link
%
-spec start_link(integer(), list()) -> {ok, pid()}.
start_link(Receive, Destination) ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, [Receive, Destination]).

%%--------------------------------------------------------------------
%
% init
%
-spec init(list()) -> {ok, list()}.
init([Receive, Destination]) ->
  % {ok, ListenSocket} = ranch_tcp:listen([{port, Receive}]),
  % Pid = spawn(?MODULE, accept, [ListenSocket, Destination]),
  % Control = make_ref(),
  Control = annex_worker_control,
  ChildSpec = [
    #{
      id => Control,
      start => {annex_worker_control, start_link, [Destination, Control]},
      restart => permanent,
      shutdown => brutal_kill,
      type => supervisor,
      modules => [annex_worker_control]
    },
    #{
      id => annex_worker_sup,
      start => {annex_worker_sup, start_link, [Receive, Control]},
      restart => permanent,
      shutdown => brutal_kill,
      type => supervisor,
      modules => [annex_worker_sup]
    }
  ],
  {ok, {{one_for_all, 6, 3600}, ChildSpec}}.

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
% fetch_source_ip
%
-spec fetch_source_ip(port()) -> bitstring().
fetch_source_ip(Socket) ->
  {ok, {Ip, _}} = inet:peername(Socket),
  ip_to_binary(Ip).

