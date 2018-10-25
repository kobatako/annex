%%%-------------------------------------------------------------------
%% @doc annex proxy sup public API
%% @end
%%%-------------------------------------------------------------------
-module(annex_proxy_sup).
-behavior(supervisor).

-export([example/0]).
-export([init/1]).
-export([start_link/1]).
-export([start_child/1]).

-type child_pid() :: term().
-type mfargs() :: {M :: module(), F :: atom(), A :: [term()] | undefined}.
-type restart() :: permanent | transient | temporary.
-type worker() :: worker | supervisor.
-type shutdown() :: brutal_kill | timeout().
-type modules() :: [module()] | dynamic.
-type child_spec() :: #{id => child_pid(),
        start => mfargs(),
        restart => restart(),
        shutdown => shutdown(),
        type => worker(),
        modules => modules()
      }.

%%====================================================================
%% API
%%====================================================================

%%--------------------------------------------------------------------
%
% start_link
%
-spec start_link(list()) -> {ok, pid()}.
start_link([]) ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, [one_for_one, 6, 3600]).

%%--------------------------------------------------------------------
%
% init
%
-spec init(list()) -> {ok, tuple()}.
init([RestartStrategy, MaxRestart, MaxTime]) ->
  {ok, { {RestartStrategy, MaxRestart, MaxTime}, [] } }.

%%--------------------------------------------------------------------
%
% start_child
%
start_child(ChildSpec) ->
  supervisor:start_child(?MODULE, ChildSpec).

%%--------------------------------------------------------------------
%
% child_spec
%
-spec child_spec(term(), term(), integer(), list()) -> child_spec().
child_spec(Pid, Proxy, Port, Hosts) ->
  #{
    id => Pid,
    start => {Proxy, start_link, [Port, Hosts]},
    restart => permanent,
    shutdown => brutal_kill,
    type => supervisor,
    modules => [Proxy]
  }.

example() ->
  Pid = make_ref(),
  ChildSpec = child_spec(Pid, annex_proxy, 5555,
    [#{host => {127, 0, 0, 1}, port => 8010}]),
  start_child(ChildSpec).

