%%%-------------------------------------------------------------------
%% @doc annex top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(annex_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

-spec start_link() -> {ok, pid()}.
start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: #{id => Id, start => {M, F, A}}
%% Optional keys are restart, shutdown, type, modules.
%% Before OTP 18 tuples must be used to specify a child. e.g.
%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
  ChildSpec = [
    #{
      id => annex_proxy_sup,
      start => {annex_proxy_sup, start_link, [[]]},
      restart => permanent,
      type => supervisor,
      modules => [annex_proxy_sup]
    }
  ],
  {ok, { {one_for_all, 0, 1}, ChildSpec} }.

%%====================================================================
%% Internal functions
%%====================================================================
