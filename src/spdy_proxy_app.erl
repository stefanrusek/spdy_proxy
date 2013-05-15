-module(spdy_proxy_app).
-behaviour(application).

-export([start/2, stop/1]).
-export([accept/2, accept/3]).

start(_StartType, _StartArgs) ->
  {ok, Options} = application:get_env(options),
  ListenPort = proplists:get_value(listen, Options),
  {ok, Sock} = gen_tcp:listen(ListenPort, [{active, false}, {packet, raw}, binary, {reuseaddr, true}]),
  spdy_proxy_sup:start_link(Sock, Options).

accept(ListenSock, Options, Parent) ->
  proc_lib:init_ack(Parent, {ok, self()}),
  accept(ListenSock, Options).
accept(ListenSock, Options) ->
  case gen_tcp:accept(ListenSock) of
    {ok, Sock} ->
      {ok, Pid} = espdy_session:start_link(Sock, gen_tcp, http_proxy, Options),
      ok = gen_tcp:controlling_process(Sock, Pid),
      Pid ! shoot;
    Err ->
      error_logger:error_info(Err)
  end,
  accept(ListenSock, Options).

stop(_State) ->
    ok.
