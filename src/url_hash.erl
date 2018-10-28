-module(url_hash).

-export([next_host/2]).
-behavior(annex_load_balancer).

next_host(Dests, #{url := #{path := Path, query := Query}}) ->
  Hash = lists:foldl(fun(U, Sum) -> U + Sum end, 0, binary_to_list(<<Path/binary, Query/binary>>)),
  Len = length(Dests),
  Dest = index_dests(Dests, Hash rem Len),
  {Dest, Dests}.

index_dests([], _) ->
  bad_match;
index_dests([Head| _], 0) ->
  Head;
index_dests([_| Tail], Index) when 1 =< Index ->
  index_dests(Tail, Index-1);
index_dests([Head| _], _)  ->
  bad_match.

