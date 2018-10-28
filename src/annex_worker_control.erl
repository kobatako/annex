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

-define(DEFAULT_OPT, #{lb => round_robin, full_match => true}).
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

-spec start_link(destinations(), atom()) -> {ok, pid()}.
start_link(Destination, Name) ->
  gen_server:start_link({local, Name}, ?MODULE, [Destination], []).

-spec init([destinations()]) -> {ok, #worker_control{}}.
init([Destination]) ->
  Dest = [default_state(Dest0) || Dest0 <- Destination],
  {ok, #worker_control{destination=Dest}}.

-spec default_state(destination()) -> destination().
default_state(Dest0) ->
  Dest1 = merge_option(Dest0),
  Dest = merge_url(Dest1),
  Dest.

-spec merge_option(destination()) -> destination().
merge_option(#{option := Opt0}=Dest) ->
  Opt = maps:merge(?DEFAULT_OPT, Opt0),
  Dest#{option := Opt};
merge_option(Dest) ->
  Dest#{option => ?DEFAULT_OPT}.

-spec merge_url(destination()) -> destination().
merge_url(#{url := Url}=Dest) ->
  Dest;
merge_url(Dest) ->
  Dest#{url => ?DEFAULT_URL}.

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

handle_call({fetch_dest, Url}, _From, State0) ->
  {Next, State} = fetch_next_host(State0, Url),
  {reply, Next, State}.

-spec fetch_next_host(#worker_control{}, bitstring()) ->
                                          {host(), #worker_control{}} |
                                          {not_found_host, #worker_control{}}.
fetch_next_host(#worker_control{destination=Dests}=State, Url) ->
  case fetch_http_url(Dests, Url, #{}) of
    #{option := #{lb := Lb}, hosts := Hosts0} ->
      {Host, Hosts} = Lb:next_host(Hosts0),
      {Host, State};
    _ ->
      {not_found_host, State}
  end.

-spec fetch_http_url(destinations(), bitstring(), map()) -> destination().
fetch_http_url([], _, Set) ->
  Set;
fetch_http_url([#{url := Url0,option:=#{full_match:=false}}=Head| Tail], Url, Set=#{url:=Url2}) ->
  Size = bit_size(Url0),
  case split_url(Url, Size) of
    {Url0, _} when Set =:= #{} ->
      fetch_http_url(Tail, Url, Head);
    {Url0, _} when Size >= bit_size(Url2) ->
      fetch_http_url(Tail, Url, Head);
    _ ->
      fetch_http_url(Tail, Url, Set)
  end;
fetch_http_url([#{url := Url0,option:=#{full_match:=true}}=Head| Tail],  Url, Set) ->
  case Url of
    Url0 ->
      fetch_http_url(Tail, Url, Head);
    _ ->
      fetch_http_url(Tail, Url, Set)
  end;
fetch_http_url([_| Tail], Url, Set) ->
  fetch_http_url(Tail, Url, Set).

split_url(Url, Size) when Size =< bit_size(Url) ->
  <<Head:Size, Tail/bitstring>> = Url,
  {<<Head:Size>>, Tail};
split_url(_, _) ->
  {error, not_match_url}.

