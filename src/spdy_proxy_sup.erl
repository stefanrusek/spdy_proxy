
-module(spdy_proxy_sup).

-behaviour(supervisor).

%% API
-export([start_link/2]).

%% Supervisor callbacks
-export([init/1, start_link_acceptor/2]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link(ListenSock, Options) ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, [ListenSock, Options]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([ListenSock, Options]) ->
  spawn_link(fun() -> initial_acceptors(Options) end),
  {ok, { {simple_one_for_one, 60, 3600},
    [
        {client, {spdy_proxy_sup, start_link_acceptor, [ListenSock, Options]}, temporary, brutal_kill, worker, [spdy_proxy_app]}
    ]
  }}.

start_link_acceptor(ListenSock, Options) ->
  proc_lib:start_link(spdy_proxy_app, accept, [ListenSock, Options, self()]).

start_acceptor() ->
  supervisor:start_child(?MODULE, []).

initial_acceptors(Options) ->
  DefaultListeners = cpu_sup:nprocs() * 2,
  Listeners = proplists:get_value(listeners, Options, DefaultListeners),
  [start_acceptor() || _ <- lists:seq(1, Listeners)],
  ok.

