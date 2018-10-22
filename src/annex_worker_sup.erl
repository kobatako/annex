-module(annex_worker_sup).

-export([start_link/2]).
-export([init/1]).

start_link(Receive, Control) ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, [Receive, Control]).

init([Receive, Control]) ->
  {ok, Listen} = ranch_tcp:listen([{port, Receive}]),
  spawn(fun() -> start_workers(Listen, Control) end),
  {ok, { {one_for_one, 6, 60}, []} }.

start_workers(Listen, Control) ->
  [start_worker(Listen, Control) || _ <- lists:seq(1, 5)].

start_worker(Listen, Control) ->
  Pid = make_ref(),
  ChildSpec = #{
    id => Pid,
    start => {annex_worker, start_link, [Listen, Control, Pid]},
    restart => temporary,
    shutdown => brutal_kill,
    type => worker,
    modules => [annex_worker]
  },
  io:format("child spec ~p~n", [ChildSpec]),
  supervisor:start_child(?MODULE, ChildSpec).

