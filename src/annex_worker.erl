-module(annex_worker).
-behavior(gen_server).

-export([start_link/3]).
-export([init/1]).
-export([handle_info/2]).
-export([handle_cast/2]).
-export([handle_call/3]).
-export([terminate/2]).

-record(worker, {
  pid,
  control,
  listen,
  front,
  back
}).

start_link(Listen, Control, Pid) ->
  io:format("work start link ~p~n", [Listen]),
  gen_server:start_link({global, Pid}, ?MODULE, [Listen, Control, Pid], []).

init([Listen, Control, Pid]) ->
  io:format("work listen ~p~n", [Listen]),
  gen_server:cast(self(), accept),
  {ok, #worker{listen=Listen,control=Control,pid=Pid}}.

handle_cast(accept, #worker{listen=Listen, control=Control}=State) ->
  case ranch_tcp:accept(Listen, infinity) of
    {ok, Socket} ->
      io:format("accept Socket ~p, controller ~p~n", [Socket, Control]),
      ranch_tcp:controlling_process(Socket, self()),
      inet:setopts(Socket, [{active, once}]),
      [{host, Host}, {port, Port}] =
          annex_worker_control:fetch_destination(Control),

      {ok, Con} = gen_tcp:connect(Host, Port, [binary, {packet, 0}]),

      {noreply, #worker{front=Socket,back=Con}};

    {error, closed} ->
      {stop, closed, State};
    {error, Reson} ->
      {stop, Reson, State}
  end.

handle_call(_, _From, State) ->
  {reply, not_found, State}.

terminate(normal, _State) ->
  io:format("terminate~n"),
  ok.

handle_info({tcp, Socket, Message}, #worker{front=Socket,back=Back}=State) ->
  gen_tcp:send(Back, Message),
  {noreply, State};
handle_info({tcp_closed, Socket}, #worker{front=Socket,back=Back}=State) ->
  ranch_tcp:close(Back),
  {stop, normal, State};
handle_info({tcp_error, Socket}, #worker{front=Socket,back=Back}=State) ->
  {stop, normal, State};

handle_info({tcp, Socket, Message}, #worker{front=Front,back=Socket}=State) ->
  gen_tcp:send(Front, Message),
  {noreply, State};
handle_info({tcp_closed, Socket}, #worker{front=Front,back=Socket}=State) ->
  ranch_tcp:close(Front),
  {stop, normal, State};
handle_info({tcp_error, Socket}, #worker{front=Front,back=Socket}=State) ->
  {stop, normal, State}.


