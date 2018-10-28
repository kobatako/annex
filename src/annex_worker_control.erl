%%%-------------------------------------------------------------------
%% @doc annex worker control API
%% @end
%%%-------------------------------------------------------------------
-module(annex_worker_control).
-behavior(gen_server).

-export([start_link/2]).
-export([init/1]).
-export([handle_info/2]).
-export([handle_cast/2]).
-export([handle_call/3]).
-export([terminate/2]).
-export([fetch_destination/2]).
-export([make_worker/2]).

-define(DEFAULT_OPT, #{lb => round_robin, full_match => false}).
-define(DEFAULT_URL, <<"/">>).

-record(worker_control, {
  destination,
  opts
}).

-type host() :: #{
  host => {integer(),integer(), integer(), integer()},
  port => integer()
}.
-type hosts() :: host().

-type option() :: #{
  lb => atom(),
  full_match => boolean()
}.
-type destination() :: #{
  url => bitstring(),
  hosts => hosts(),
  option => option()
}.
-type destinations() :: [destination()].

%%====================================================================
%% API
%%====================================================================

%%--------------------------------------------------------------------
%
% start_link
%
-spec start_link(destinations(), atom()) -> {ok, pid()}.
start_link(Destinations, Name) ->
  gen_server:start_link({local, Name}, ?MODULE, [Destinations], []).

%%--------------------------------------------------------------------
%
% init
%
-spec init([destinations()]) -> {ok, #worker_control{}}.
init([Destinations]) ->
  Dest = [default_state(Dest0) || Dest0 <- Destinations],
  {ok, #worker_control{destination=Dest}}.

fetch_destination(Pid, Url) ->
  gen_server:call(Pid, {fetch_dest, Url}).

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

handle_call({fetch_dest, Url}, _From, State) ->
  {Next, Dests} = fetch_next_host(State, Url),
  {reply, Next, State#worker_control{destination=Dests}}.


%%====================================================================
%% Internal functions
%%====================================================================

%%--------------------------------------------------------------------
%
% default_state
%
-spec default_state(destination()) -> destination().
default_state(Dest0) ->
  Dest1 = merge_option(Dest0),
  merge_url(Dest1).

%%--------------------------------------------------------------------
%
% merge_option
%
-spec merge_option(destination()) -> destination().
merge_option(#{option := Opt0}=Dest) ->
  Opt = maps:merge(?DEFAULT_OPT, Opt0),
  Dest#{option := Opt};
merge_option(Dest) ->
  Dest#{option => ?DEFAULT_OPT}.

%%--------------------------------------------------------------------
%
% merge_url
%
-spec merge_url(destination()) -> destination().
merge_url(#{url := Url}=Dest) ->
  Dest;
merge_url(Dest) ->
  Dest#{url => ?DEFAULT_URL}.

%%--------------------------------------------------------------------
%
% fetch_next_host
%
-spec fetch_next_host(#worker_control{}, term()) ->
                                          {host(), #worker_control{}} |
                                          {not_found_host, #worker_control{}}.
fetch_next_host(#worker_control{destination=Dests}=State, #{path := Path}=Url) ->
  case fetch_http_url(Dests, Path) of
    {#{option := #{lb := Lb}, hosts := Hosts0}=Dest, Head, Tail} ->
      {Host, Hosts} = Lb:next_host(Hosts0, #{url => Url}),
      {Host, Head ++ [Dest#{hosts:=Hosts}] ++ Tail};
    _ ->
      {not_found_host, State}
  end.

%%--------------------------------------------------------------------
%
% fetch_http_url
%
-spec fetch_http_url(destinations(), bitstring()) ->
                        {destination(), destinations(), destinations()}.
fetch_http_url([], _) ->
  {};
fetch_http_url(State, Url) ->
  fetch_http_url(State, Url, []).


-spec fetch_http_url(destinations(), bitstring(), destinations()) ->
                        {destination(), destinations(), destinations()}.
fetch_http_url([], _, _) ->
  {};
fetch_http_url([#{url := Url0,option:=#{full_match:=false}}=Head| Tail], Url, Heads) ->
  Size = bit_size(Url0),
  case split_url(Url, Size) of
    {Url0, _} ->
      {Head, lists:reverse(Heads), Tail};
    _ ->
      fetch_http_url(Tail, Url, [Head| Heads])
  end;
fetch_http_url([#{url := Url, option:=#{full_match:=true}}=Head| Tail], Url, Heads) ->
  {Head, lists:reverse(Heads), Tail};
fetch_http_url([Head| Tail], Url, Heads) ->
  fetch_http_url(Tail, Url, [Head| Heads]).

%%--------------------------------------------------------------------
%
% split url by size
%
split_url(Url, Size) when Size =< bit_size(Url) ->
  <<Head:Size, Tail/bitstring>> = Url,
  {<<Head:Size>>, Tail};
split_url(_, _) ->
  {error, not_match_url}.

