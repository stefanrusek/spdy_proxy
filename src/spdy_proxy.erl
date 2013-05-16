-module(spdy_proxy).

-export([main/1]).

-define(DEFAULTS, [{listen, 9999}, {port, 80}, {server, "localhost"}, {spdy_version, negotiate}, {action, fun start/1}]).

parse_args([], Options) -> lists:reverse(Options);

parse_args(["-h" | Args], Options) -> parse_args(["--help" | Args], Options);
parse_args(["--help" | Args], Options) -> parse_args(Args, [{action, fun help/1} | Options]);

parse_args(["-l" | Args], Options) -> parse_args(["--listen" | Args], Options);
parse_args(["--listen", Port | Args], Options) -> parse_args(Args, [{listen, list_to_integer(Port)} | Options]);

parse_args(["-c" | Args], Options) -> parse_args(["--listeners" | Args], Options);
parse_args(["--count" | Args], Options) -> parse_args(["--listeners" | Args], Options);
parse_args(["--listeners", Count | Args], Options) -> parse_args(Args, [{listeners, list_to_integer(Count)} | Options]);

parse_args(["-p" | Args], Options) -> parse_args(["--port" | Args], Options);
parse_args(["--port", Port | Args], Options) -> parse_args(Args, [{port, list_to_integer(Port)} | Options]);

parse_args(["-s" | Args], Options) -> parse_args(["--server" | Args], Options);
parse_args(["--server", Host | Args], Options) -> parse_args(Args, [{server, Host} | Options]);

parse_args(["-x" | Args], Options) -> parse_args(["--spdyversion" | Args], Options);
parse_args(["--spdyversion", "3" | Args], Options) -> parse_args(Args, [{spdy_version, 3} | Options]);
parse_args(["--spdyversion", "2" | Args], Options) -> parse_args(Args, [{spdy_version, 2} | Options]);
parse_args(["--spdyversion", "negotiate" | Args], Options) -> parse_args(Args, [{spdy_version, negotiate} | Options]);

parse_args(["-v" | Args], Options) -> parse_args(["--verbose" | Args], Options);
parse_args(["--verbose" | Args], Options) -> restart_logging(), parse_args(Args, Options);

parse_args([Unrecognized | Args], Options) ->
  parse_args(Args, [{error, "Unrecognized: " ++ Unrecognized} | Options]).

merge([], Out) -> Out;
merge([{Key, Value} | In], Out) ->
  case proplists:get_value(Key, Out) of
    undefined -> merge(In, [{Key, Value} | Out]);
    _ -> merge(In, Out)
  end.

dev_null(Parent) ->
  receive
    {io_request, From, ReplyAs, _Request} -> From ! {io_reply, ReplyAs, ok};
    {notify, _} -> ok;
    M -> 
      case Parent of
        null ->
          io:format("UNEXPECTED: ~p~n", [M]);
        Pid ->
          Pid ! M
      end
  end,
  dev_null(Parent).

stop_logging() ->
  Spdy = spawn(fun() -> dev_null(null) end),
  register(spdy_logging, Spdy),

  ErrorLogger = whereis(error_logger),
  NullLogger = spawn(fun() -> dev_null(ErrorLogger) end),
  unregister(error_logger),
  register(error_logger, NullLogger),
  register(error_logger_bak, ErrorLogger), 
  ok.

restart_logging() ->
  case whereis(spdy_logging) of
    Pid when is_pid(Pid) ->
      io:format("Entering Verbose mode"),
      unregister(spdy_logging),
      unregister(error_logger),
      ErrorLogger = whereis(error_logger_bak),
      unregister(error_logger_bak),
      register(error_logger, ErrorLogger)
  end.

main(Args) ->
  io:format("spdy_proxy - simple, scalable SPDY -> HTTP proxy\n"),

  stop_logging(),
  PassedOptions = parse_args(Args, []),

  ok = application:set_env(sasl, sasl_error_logger, false),
  ok = application:set_env(sasl, errlog_type, error),
  ok = error_logger:tty(false),

  application:start(sasl),
  application:start(os_mon),
  application:start(inets),
  application:start(espdy),

  Options = merge(?DEFAULTS, PassedOptions),
  Action = case proplists:get_value(error, Options) of
    undefined -> proplists:get_value(action, Options);
    _ -> fun help/1
  end,
  Action(Options).

start(Options) ->
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
      "  -x, --spdyversion SPDY version (default negotiate)",
      "  -v, --verbose     Print way too much information"
    ],
  Output = case proplists:get_value(error, Options) of
    undefined -> Message;
    Error -> [Error | Message]
  end,
  io:format("~s~n", [string:join(Output, "\n")]).
