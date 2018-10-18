%%%-------------------------------------------------------------------
%% @doc annex http
%% @end
%%%-------------------------------------------------------------------

-module(annex_http).

-export([parse/1]).
-export([parse_method/2]).
-export([parse_uri/2]).
-export([parse_header/2]).
-export([add_x_forwarded_for/2]).
-export([fetch_x_forwarded_for/1]).

-define(BLANK, $\s).

-define(GET, <<"GET">>).
-define(POST, <<"POST">>).
-define(PUT, <<"PUT">>).
-define(DELETE, <<"DELETE">>).
-define(IS_HTTP_METHOD(H),
              (H =:= ?GET) or (H =:= ?POST)
           or (H =:= ?PUT) or (H =:= ?DELETE)).

-define(X_FORWARDED_FOR, <<"X-Forwarded-For">>).

-type http_version() :: http_1_0
                      | http_1_1
                      | http_2_0.

-type http_format() :: #{method => bitstring(),
                          uri => bitstring(),
                          version => http_version(),
                          header => list()}.

%%====================================================================
%% API
%%====================================================================

%%--------------------------------------------------------------------
% @doc parse http format
% parse
%
-spec parse(bitstring()) -> http_format().
parse(Buffer) ->
  {ok, Method, Res0} = parse_method(Buffer, <<>>),
  {ok, Uri, Res1} = parse_uri(Res0, <<>>),
  {ok, Ver, Res2} = parse_version(Res1),
  {ok, Header} = parse_header(Res2, []),
  #{method => Method, uri => Uri, version => Ver, header => Header}.

%%--------------------------------------------------------------------
% @doc parse http header method
% parse_method
%
-spec parse_method(bitstring(), bitstring()) -> {ok, bitstring(), bitstring()}.
parse_method(<<?BLANK, Res/binary>>, Method) when ?IS_HTTP_METHOD(Method) ->
  {ok, Method, Res};
parse_method(<<C, Res/binary>>, Method) ->
  parse_method(Res, <<Method/binary, C>>).

%%--------------------------------------------------------------------
% @doc parse http header uri
% parse_uri
%
-spec parse_uri(bitstring(), bitstring()) -> {ok, bitstring(), bitstring()}.
parse_uri(<<?BLANK, Res/binary>>, Uri) ->
  {ok, Uri, Res};
parse_uri(<<C, Res/binary>>, Uri) ->
  parse_uri(Res, <<Uri/binary, C>>).

%%--------------------------------------------------------------------
%
% parse_version
%
-spec parse_version(bitstring()) -> {ok, http_version(), bitstring()}
                                  | {error, atom(), bitstring()}.
parse_version(<<"HTTP/1.0\r\n", Res/binary>>) ->
  {ok, http_1_0, Res};
parse_version(<<"HTTP/1.1\r\n", Res/binary>>) ->
  {ok, http_1_1, Res};
parse_version(<<"HTTP/2.0\r\n", Res/binary>>) ->
  {ok, http_2_0, Res};
parse_version(Res) ->
  {error, not_match_http_version, Res}.

%%--------------------------------------------------------------------
%
% parse_header
%
-spec parse_header(bitstring(), list()) -> {ok, list()}.
parse_header(<<$\r, $\n, $\r, $\n>>, Headers) ->
  {ok, Headers};
parse_header(<<$\r, $\n, _/binary>>, Headers) ->
  {ok, Headers};
parse_header(<<Res0/binary>>, Headers) ->
  {ok, Header, Res} = parse_header_attr(Res0, <<>>),
  parse_header(Res, [Header| Headers]).

%%--------------------------------------------------------------------
%
% add x forwarded for
%
-spec add_x_forwarded_for(map(), bitstring()) -> list().
add_x_forwarded_for(#{header := Header}, Ip) when is_binary(Ip) ->
  case fetch_x_forwarded_for(Header) of
    not_found ->
      [{<<?X_FORWARDED_FOR/binary>>, Ip}| Header];
    Forwarded ->
      [{<<?X_FORWARDED_FOR/binary>>, <<Forwarded/binary, ", ", Ip/binary>>}|
        Header]
  end.

%%--------------------------------------------------------------------
%
% fetch x forwarded for
%
-spec fetch_x_forwarded_for(list()) -> bitstring() | atom().
fetch_x_forwarded_for([]) ->
  not_found;
fetch_x_forwarded_for([{Attr, Value}| Tail]) ->
  AttrLow = string:lowercase(Attr),
  case string:lowercase(?X_FORWARDED_FOR) of
    AttrLow ->
      Value;
    _ ->
      fetch_x_forwarded_for(Tail)
  end.

%%====================================================================
%% Internal functions
%%====================================================================

%%--------------------------------------------------------------------
%
% parse header attr
%
-spec parse_header_attr(bitstring(), bitstring()) ->
                                {ok, {bitstring(), bitstring()}, bitstring()}
                              | {error, atom()}.
parse_header_attr(<<>>, _) ->
  {error, not_header_attr};
parse_header_attr(<<$:, Res0/binary>>, Attr) ->
  {ok, Value, Res} = parse_header_value(Res0, <<>>),
  {ok, {Attr, string:trim(Value)}, Res};
parse_header_attr(<<C, Res/binary>>, Attr) ->
  parse_header_attr(Res, <<Attr/binary, C>>).

%%--------------------------------------------------------------------
%
% parse header value
%
-spec parse_header_value(bitstring(), bitstring()) ->
                                {ok, bitstring(), bitstring()}
                              | {error, atom()}.
parse_header_value(<<>>, _) ->
  {error, not_header_value};
parse_header_value(<<$\r, $\n, Res/binary>>, Value) ->
  {ok, Value, Res};
parse_header_value(<<C, Res/binary>>, Value) ->
  parse_header_value(Res, <<Value/binary, C>>).


