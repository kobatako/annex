-module(round_robin).

-export([next_host/2]).
-behavior(annex_load_balancer).

next_host([Dest| Tail], _) ->
  {Dest, Tail ++ [Dest]}.

