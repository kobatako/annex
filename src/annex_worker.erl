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

%%====================================================================
%% API functions
%%====================================================================

start_link(Listen, Control, Pid) ->
  gen_server:start_link({global, Pid}, ?MODULE, [Listen, Control, Pid], []).

init([Listen, Control, Pid]) ->
  gen_server:cast(self(), accept),
  {ok, #worker{listen=Listen,control=Control,pid=Pid}}.

handle_cast(accept, #worker{listen=Listen, control=Control}=State) ->
  case ranch_tcp:accept(Listen, infinity) of
    {ok, Socket} ->
      ranch_tcp:controlling_process(Socket, self()),
      inet:setopts(Socket, [{active, once}]),

      annex_worker_control:make_worker(Control, Listen),
      {noreply, #worker{front=Socket,control=Control,listen=Listen}};
    {error, closed} ->
      {stop, closed, State};
    {error, Reson} ->
      {stop, Reson, State}
  end.

handle_call(_, _From, State) ->
  {reply, not_found, State}.

terminate(normal, _State) ->
  ok.

handle_info({tcp, Socket, Message}, #worker{front=Socket,control=Control}=State) ->
  #{uri := Url} = annex_http:parse(Message),
  case annex_worker_control:fetch_destination(Control, Url) of
    #{host := Host, port := Port} ->
      {ok, Con} = gen_tcp:connect(Host, Port, [binary, {packet, 0}]),
      gen_tcp:send(Con, Message),
      {noreply, State#worker{back=Con}};
    _ ->
      {stop, normal, not_found_next_host}
  end;
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


