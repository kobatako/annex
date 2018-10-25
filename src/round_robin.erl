-module(round_robin).

-export([next_host/1]).
-behavior(annex_load_balancer).

next_host([Dest| Tail]) ->
  {Dest, Tail ++ [Dest]}.

