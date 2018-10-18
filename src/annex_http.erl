-module(annex_http).

-export([parse/1]).

-define(BLANK, $\s).
-define(GET, <<GET>>).
-define(POST, <<POST>>).
-define(PUT, <<PUT>>).
-define(DELETE, <<DELETE>>).


parse(Buffer) ->
  {ok, Method, Res0} = parse_method(Buffer, <<>>),
  {ok, Uri, Res1} = parse_uri(Res0, <<>>),
  {ok, Ver, Res2} = parse_version(Res1),
  {ok, Header} = parse_header(Res2, []),
  #{method => Method, uri => Uri, version => Ver, header => Header}.

parse_method(<<?BLANK, Res/binary>>, Method) ->
  {ok, Method, Res};
parse_method(<<C, Res/binary>>, Method) ->
  parse_method(Res, <<Method/binary, C>>).

parse_uri(<<?BLANK, Res/binary>>, Uri) ->
  {ok, Uri, Res};
parse_uri(<<C, Res/binary>>, Uri) ->
  parse_uri(Res, <<Uri/binary, C>>).

parse_version(<<"HTTP/1.1\r\n", Res/binary>>) ->
  {ok, "HTTP/1.1", Res};
parse_version(<<"HTTP/1.0\r\n", Res/binary>>) ->
  {ok, "HTTP/1.0", Res};
parse_version(<<"HTTP/2.0\r\n", Res/binary>>) ->
  {ok, "HTTP/2.0", Res};
parse_version(Res) ->
  {error, not_match_http_version, Res}.

parse_header(<<$\r, $\n, $\r, $\n>>, Headers) ->
  {ok, Headers};
parse_header(<<$\r, $\n, Res/binary>>, Headers) ->
  {ok, Headers};
parse_header(<<Res0/binary>>, Headers) ->
  {ok, Header, Res} = parse_header_attr(Res0, <<>>),
  parse_header(Res, [Header| Headers]).

parse_header_attr(<<>>, _) ->
  {error, not_header_attr};
parse_header_attr(<<$:, Res0/binary>>, Attr) ->
  {ok, Value, Res} = parse_header_value(Res0, <<>>),
  {ok, {Attr, string:trim(Value)}, Res};
parse_header_attr(<<C, Res/binary>>, Attr) ->
  parse_header_attr(Res, <<Attr/binary, C>>).

parse_header_value(<<>>, _) ->
  {error, not_header_value};
parse_header_value(<<$\r, $\n, Res/binary>>, Value) ->
  {ok, Value, Res};
parse_header_value(<<C, Res/binary>>, Value) ->
  parse_header_value(Res, <<Value/binary, C>>).

