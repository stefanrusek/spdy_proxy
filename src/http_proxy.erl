-module(http_proxy).
-include("deps/espdy/include/espdy.hrl").

-export([preprocess_opts/1]).
-export([init/3, closed/2, headers_updated/3, handle_data/2, handle_info/2]). %% API

-define(HOST_RE, "(:\\d+)?$").

preprocess_opts(Opts) ->
  [{http_proxy_host_re, re:compile(?HOST_RE)} | Opts].

% header name for spdy version
scheme(_Version = 2, Headers) -> proplists:get_value(<<"scheme">>, Headers);
scheme(_Version = 3, Headers) -> proplists:get_value(<<":scheme">>, Headers).

host(_Version = 2, Headers) -> proplists:get_value(<<"host">>, Headers);
host(_Version = 3, Headers) -> proplists:get_value(<<":host">>, Headers).

method(_Version = 2, Headers) -> proplists:get_value(<<"method">>, Headers);
method(_Version = 3, Headers) -> proplists:get_value(<<":method">>, Headers).

path(_Version = 2, Headers) -> proplists:get_value(<<"url">>, Headers);
path(_Version = 3, Headers) -> proplists:get_value(<<":path">>, Headers).

status(_Version = 2, Value) -> {<<"status">>, Value};
status(_Version = 3, Value) -> {<<":status">>, Value}.

version(_Version = 2, Headers) when is_list(Headers) -> proplists:get_value(<<"version">>, Headers);
version(_Version = 3, Headers) when is_list(Headers) -> proplists:get_value(<<":version">>, Headers);
version(_Version = 2, Value) -> {<<"version">>, Value};
version(_Version = 3, Value) -> {<<":version">>, Value}.

format_http_headers(Headers) ->
  binary:list_to_bin([[K, <<": ">>, V, <<"\n">>] || {K, V} <- Headers]).

init(_Id, Headers, SpdyOpts) ->
  SpdyVersion = proplists:get_value(spdy_version, SpdyOpts),
  HostAndPort = host(SpdyVersion, Headers),
  Host = lists:flatten(re:replace(HostAndPort, proplists:get_value(http_proxy_host_re, SpdyOpts, ?HOST_RE), "")),
  Method = method(SpdyVersion, Headers),
  Path = path(SpdyVersion, Headers),
  HttpVersion = version(SpdyVersion, Headers), 
  ProxyPort = proplists:get_value(port, SpdyOpts),
  ProxyServer = proplists:get_value(server, SpdyOpts),
  case gen_tcp:connect(ProxyServer, ProxyPort, [{mode, list}, {active, once}, {packet, line}]) of
    {ok, Sock} ->
      NewHeaders = [{<<"host">>, Host} | proplists:delete(<<"host">>, Headers)],
      FirstPacket = io_lib:format("~s ~s ~s~n~s~n", [Method, Path, HttpVersion, format_http_headers(NewHeaders)]),
      case gen_tcp:send(Sock, FirstPacket) of
        ok -> {ok, noreply, [{spdy_version, SpdyVersion}, {sock, Sock}, {response_headers, []}]};
        _ -> 
          gen_tcp:close(Sock),
          {error, ?INTERNAL_ERROR}
      end;
    _ -> {error, ?REFUSED_STREAM}
  end.

%% Called when the SPDY session terminates
closed(Reason, State) ->
  case proplists:get_value(sock, State) of
    undefined -> ok;
    Sock -> gen_tcp:close(Sock)
  end,
  ?LOG("CLOSED! ~p\n",[Reason]).

%% Called when a HEADERS frame updated the headers
headers_updated(_Delta, _NewMergedHeaders, State) ->
  ?LOG("headers updated with ~p",[_Delta]),
  {ok, State}.

%% Called when we recieve a DATA frame
handle_data(Data, State) ->
  ?LOG("DATA on stream ~p",[Data]),
  gen_tcp:send(proplists:get_value(sock, State), Data),
  {ok, State}.

handle_info({tcp, Sock, "\r\n"}, State) ->
  handle_info({tcp, Sock, ""}, State);
handle_info({tcp, Sock, ""}, State) ->
  OutHeaders = proplists:get_value(response_headers, State),
  ?LOG("Response Headers ~p~n", [OutHeaders]),
  espdy_stream:send_frame(self(), #spdy_syn_reply{ headers = OutHeaders }),
  inet:setopts(Sock, [{packet, raw}, {active, true}, {mode, binary}]),
  {noreply, State};

handle_info({tcp, Sock, Line}, State) when is_list(Line) ->
  Headers = case proplists:get_value(response_headers, State) of
    [] -> 
      SpdyVersion = proplists:get_value(spdy_version, State),
      case Line of
        "HTTP/1.1 " ++ Status1 -> [status(SpdyVersion, strip_crlf(Status1)), version(SpdyVersion, <<"HTTP/1.1">>)];
        "HTTP/1.0 " ++ Status2 -> [status(SpdyVersion, strip_crlf(Status2)), version(SpdyVersion, <<"HTTP/1.0">>)]
      end;
    PL ->
      case Line of
        " " ++ Continue ->
          [{K, V} | Tail] = PL,
          [{K, << V/binary, <<" ">>/binary, (clean_spaces(Continue))/binary >>} | Tail];
        Line ->
          [split_header(Line) | PL]
      end
  end,
  inet:setopts(Sock, [{active, once}]),
  {noreply, [{response_headers, Headers} | proplists:delete(response_headers, State)]};
handle_info({tcp, _Sock, Bin}, State) ->
  espdy_stream:send_data(self(), Bin),
  {noreply, State};
handle_info({tcp_closed, _Sock}, State) ->
  espdy_stream:send_data_fin(self()),
  {noreply, State};
handle_info({tcp_error, _Sock, _Err}, State) ->
  espdy_stream:send_data_fin(self()),
  {noreply, State};

  
handle_info(_M, State) ->
  ?LOG("~p", [_M]),
  {noreply, State}.

split_header(Line) ->
  split_header(Line, {start, []}).
clean_spaces(Line) ->
  {_, Cleaned} = split_header(Line, {tail, []}),
  Cleaned.

split_header(":" ++ Value, {start, Name}) -> split_header(Value, {tail, list_to_binary(string:to_lower(lists:reverse(Name)))});
split_header([C | Rest], {start, Name}) -> split_header(Rest, {start, [C | Name]});
split_header(" " ++ Value, Acc) -> split_header(Value, Acc);
split_header(Value, {tail, Name}) -> {Name, strip_crlf(Value)}.

strip_crlf(Line) -> strip_crlf(Line, []).
strip_crlf([], Acc) -> list_to_binary(lists:reverse(Acc));
strip_crlf("\r" ++ Line, Acc) -> strip_crlf(Line, Acc);
strip_crlf("\n" ++ Line, Acc) -> strip_crlf(Line, Acc);
strip_crlf([C | Line], Acc) -> strip_crlf(Line, [C | Acc]).
