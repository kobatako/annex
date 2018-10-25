-module(annex_worker_control).
-behavior(gen_server).

-export([start_link/3]).
-export([init/1]).
-export([handle_info/2]).
-export([handle_cast/2]).
-export([handle_call/3]).
-export([terminate/2]).
-export([fetch_destination/1]).
-export([make_worker/2]).

-define(DEFAULT_OPT, #{lb=>round_robin}).

-record(worker_control, {
  destination,
  opts
}).

start_link(Destination, Name, Opts) ->
  gen_server:start_link({local, Name}, ?MODULE, [Destination, Opts], []).

init([Destination, Opts0]) ->
  Opts = maps:merge(?DEFAULT_OPT, Opts0),
  {ok, #worker_control{destination=Destination,opts=Opts}}.

fetch_destination(Pid) ->
  gen_server:call(Pid, {fetch_dest}).

make_worker(Pid, Listen) ->
  gen_server:cast(Pid, {make_worker, Listen, Pid}).


handle_info(_, State) ->
  {noreply, State}.

handle_cast({make_worker, Listen, Control}, State) ->
  annex_worker_sup:start_worker(Listen, Control),
  {noreply, State};

handle_cast(_, State) ->
  {noreply, State}.

terminate(_Message, _State) ->
  ok.

handle_call({fetch_dest}, _From, State) ->
  {Next, Dest} = fetch_dest(State),
  {reply, Next, State#worker_control{destination=Dest}}.

fetch_dest(#worker_control{destination=Dest,opts=#{lb:=Lb}}) ->
  Lb:next_host(Dest).

