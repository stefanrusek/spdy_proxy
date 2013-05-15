-module(spdy_proxy).

-export([main/1]).

-define(DEFAULTS, [{listen, 9999}, {port, 80}, {server, "localhost"}, {spdy_version, negotiate}, {action, fun start/1}]).

parse_args([], Options) -> lists:reverse(Options);

parse_args(["-h" | Args], Options) -> parse_args(["--help" | Args], Options);
parse_args(["--help" | _], Options) -> [{action, fun help/1} | Options];

parse_args(["-l" | Args], Options) -> parse_args(["--listen" | Args], Options);
parse_args(["--listen", Port | _], Options) -> [{listen, list_to_integer(Port)} | Options];

parse_args(["-c" | Args], Options) -> parse_args(["--listeners" | Args], Options);
parse_args(["--count" | Args], Options) -> parse_args(["--listeners" | Args], Options);
parse_args(["--listeners", Count | _], Options) -> [{listeners, list_to_integer(Count)} | Options];

parse_args(["-p" | Args], Options) -> parse_args(["--port" | Args], Options);
parse_args(["--port", Port | _], Options) -> [{port, list_to_integer(Port)} | Options];

parse_args(["-s" | Args], Options) -> parse_args(["--server" | Args], Options);
parse_args(["--server", Host | _], Options) -> [{server, Host} | Options];

parse_args(["-x" | Args], Options) -> parse_args(["--spdyversion" | Args], Options);
parse_args(["--spdyversion", "3" | _], Options) -> [{spdy_version, 3} | Options];
parse_args(["--spdyversion", "2" | _], Options) -> [{spdy_version, 2} | Options];
parse_args(["--spdyversion", "negotiate" | _], Options) -> [{spdy_version, negotiate} | Options];

parse_args([Unrecognized | Args], Options) ->
  parse_args(Args, [{error, "Unrecognized: " ++ Unrecognized} | Options]).

merge([], Out) -> Out;
merge([{Key, Value} | In], Out) ->
  case proplists:get_value(Key, Out) of
    undefined -> merge(In, [{Key, Value} | Out]);
    _ -> merge(In, Out)
  end.

main(Args) ->
  PassedOptions = parse_args(Args, []),
  Options = merge(?DEFAULTS, PassedOptions),
  Action = case proplists:get_value(error, Options) of
    undefined -> proplists:get_value(action, Options);
    _ -> fun help/1
  end,
  Action(Options).

start(Options) ->
  application:start(sasl),
  application:start(os_mon),
  application:start(inets),
  application:start(espdy),

  application:set_env(spdy_proxy, options, Options),
  application:start(spdy_proxy, permanent),
  receive impossible_message -> ok end.

help(Options) ->
  Message = [
      "spdy_proxy - a simple scalable SPDY -> HTTP proxy",
      "  -h, --help        Prints this help message",
      "  -l, --listen      Listen on this port (default 9999)",
      "  -c, --count       Number of listener threads (default cpu_cores*2)",
      "  -p, --port        Connects on this port (default 80)",
      "  -s, --server      Connects to this server (default localhost)",
      "  -x, --spdyversion SPDY version (default negotiate)"
    ],
  Output = case proplists:get_value(error, Options) of
    undefined -> Message;
    Error -> [Error | Message]
  end,
  io:format("~s~n", [string:join(Output, "\n")]).
