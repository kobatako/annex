-module(annex_load_balancer).

-callback next_host(list(), map()) -> {term(), list()}.

