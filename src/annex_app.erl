%%%-------------------------------------------------------------------
%% @doc annex public API
%% @end
%%%-------------------------------------------------------------------

-module(annex_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

-spec start(_, _) -> {ok, pid()}.
start(_StartType, _StartArgs) ->
    annex_sup:start_link().

%%--------------------------------------------------------------------

-spec stop(_) -> ok.
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
